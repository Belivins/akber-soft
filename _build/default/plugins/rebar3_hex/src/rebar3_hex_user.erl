%% @doc `rebar3 hex user' - Hex user tasks.
%%
%% <h2> Register a user </h2>
%%
%% ```
%% $ rebar3 hex user register
%% '''
%% 
%% <h2> Print the current user </h2>
%%
%% ```
%% $ rebar3 hex user whoami
%% '''
%%
%% <h2> Authorize a new user </h2>
%%
%% ```
%% $ rebar3 hex user auth [--key-name KEY_NAME]
%% '''
%%
%% <h2> Deauthorize the user </h2>
%%
%% Deauthorizes the user from the local machine by removing the API key from the Hex config.
%%
%% ```
%% $ rebar3 hex user deauth
%% ''' 
%%
%% <h2>Generate user key</h2>
%%
%% Generates an unencrypted API key for your account. Keys generated by this command will be owned by you and will 
%% give access to your private resources, do not share this key with anyone. For keys that will be shared by 
%% organization members use `rebar3 hex organization key' instead. By default this command sets the api:write 
%% permission which allows write access to the API, it can be overridden with the --permission flag.
%% 
%% ```
%% $ rebar3 hex user key generate [--key-name KEY_NAME] [--permission PERMISSION]
%% '''
%%
%% <h2> Revoke key </h2>
%% Removes given key from account.
%% The key can no longer be used to authenticate API requests.
%%
%% ```
%% $ rebar3 hex user key revoke NAME KEY_NAME
%% '''
%%
%% == Revoke all keys == 
%% Revoke all keys from your account.
%%
%% ```
%% $ rebar3 hex user key revoke --all
%% '''
%%
%% <h2> List keys </h2>
%% Lists all keys associated with the organization.
%%
%% ```
%% $ rebar3 hex user key list
%% '''
%%
%% <h2> Reset user account password  </h2>
%% 
%% Starts the process for resetting account password.
%%
%% ```
%% rebar3 hex user reset_password account 
%% '''
%%
%% <h2>  Reset local password </h2>
%%
%% ```
%% rebar3 hex user reset_password local 
%% '''
%% 
%% <h2> Command line options </h2>
%%
%% <ul>
%%   <li>
%%       `--repo' - Specify the repository to work with. This option is required when 
%%       you have multiple repositories configured, including organizations. The argument must be a fully qualified 
%%       repository name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%       Default to `hexpm'.
%%   </li>
%%   <li>
%%      `--message "MESSAGE"' - Required message (up to 140 characters) clarifying the retirement reason
%%   </li>
%%   <li>
%%      `--key-name KEY_NAME' - By default Hex will base the key name on your machine's hostname and the organization 
%%      name, use this option to give your own name.
%%   </li>
%%    <li>
%%      `--permission PERMISSION' -  Sets the permissions on the key, this option can be given multiple times,
%%          possible values are:
%%      <ul>
%%          <br/>
%%          <li>`api:read' - API read access.</li>
%%          <li>`api:write' - API write access.</li>
%%          <li>`repository:ORGANIZATION_NAME' - Access to the repository (this is the default permission).</li>
%%          <li>`repositories' - Access to repositories for all organizations you are member of.</li>
%%      </ul>
%%    </li>
%% </ul>

-module(rebar3_hex_user).

-export([init/1,
         do/1,
         format_error/1]).

-export([encrypt_write_key/3,
         decrypt_write_key/2]).

-include("rebar3_hex.hrl").

-define(PROVIDER, user).
-define(DEPS, [{default, lock}]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex user <command>"},
                                 {short_desc, "Hex user tasks"},
                                 {desc, ""},
                                 {opts, [
                                         rebar3_hex:repo_opt(),
                                         {all, $a, "all", boolean, "all."},
                                         {key_name, $k, "key-name", string, "key-name"},
                                         {permission, $p, "permission", list, "perms."}
                                        ]
                                 }]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar3_hex:task_state(State) of
        {ok, Task} ->
            handle_task(Task);
        {error, Reason} ->
            ?RAISE(Reason)
    end.

%% @private
-spec format_error(any()) -> iolist().
format_error({decrypt_write_key, no_write_key}) -> 
    "No write key found for user in this repository. "
    "Be sure you have authenticated first with : rebar3 hex user auth";

format_error({whoami, Reason}) when is_binary(Reason) ->
    io_lib:format("Fetching currently authenticated user failed: ~ts", [Reason]);
format_error({input_required, InputName}) ->
    Str = io_lib:format("The task you are attempting to run requires a ~ts. ", [InputName]),
    Str ++ io_lib:format("Try running this again and be sure to give a ~ts when prompted.", [InputName]);
format_error(bad_local_password) ->
    "Failure to decrypt write key: bad local password";
format_error({registration_failure, Errors}) when is_map(Errors) ->
    Reason = rebar3_hex_client:pretty_print_errors(Errors),
    io_lib:format("Registration of user failed: ~ts", [Reason]);
format_error({generate_key, Reason}) when is_binary(Reason) ->
    io_lib:format("Failure generating authentication tokens: ~ts", [Reason]);
format_error({key_revoke, {error, #{<<"status">> := 404}}}) ->
    "The key you tried to revoke was not found";
format_error({key_revoke_all, {error, #{<<"message">> := Msg}}}) ->
    io_lib:format("Error revoking all keys : ~ts", [Msg]);
format_error({key_list, {error, #{<<"message">> := Msg}}}) ->
    io_lib:format("Error listing keys : ~ts", [Msg]);
format_error(passwords_do_not_match) ->
    "Password confirmation failed. The passwords must match.";
format_error(local_password_too_big) ->
    "Local passwords can not exceed 32 characters.";
format_error({reset_account_password, Reason}) when is_binary(Reason) ->
    io_lib:format("Error reseting account password: ~ts", [Reason]);
format_error(not_authenticated) ->
    "Not authenticated as any user currently for this repository";
format_error(bad_command) ->
    "Invalid arguments, expected one of:\n\n"
    "rebar3 hex user register\n"
    "rebar3 hex user auth\n"
    "rebar3 hex user deauth\n"
    "rebar3 hex user whoami\n"
    "rebar3 hex key generate\n"
    "rebar3 hex key revoke --key-name KEY_NAME\n"
    "rebar3 hex key revoke --all\n"
    "rebar3 hex key list\n"
    "rebar3 hex key fetch --key-name KEY_NAME\n"
    "rebar3 hex reset_password account\n"
    "rebar3 hex reset_password local\n";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

handle_task(#{args := #{task := register}} = Task) ->
    #{repo := Repo, state := State} = Task,
    rebar3_hex_io:say("By registering an account on Hex.pm you accept all our "
                "policies and terms of service found at https://hex.pm/policies\n"),
    Username = get_string_input("Username"),
    Email = get_string_input("Email"),
    Password = get_password(account),
    rebar3_hex_io:say("Registering..."),
    create_user(Username, Email, Password, Repo, State);

handle_task(#{args := #{task := auth}} = Task) ->
    #{repo := #{repo_name := RepoName} = Repo, state := State} = Task,
    Username = get_string_input("Username"),
    Password = get_password(account),

    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    RepoConfig0 = Repo#{api_key => rebar_utils:to_binary("Basic " ++ Auth)},

    %% write key
    WriteKeyName = api_key_name(),
    WritePermissions = [#{<<"domain">> => <<"api">>}],
    WriteKey = generate_key(RepoConfig0, WriteKeyName, WritePermissions),

    rebar3_hex_io:say("You have authenticated on Hex using your account password. However, "
                "Hex requires you to have a local password that applies only to this machine for security "
                "purposes. Please enter it."),


    LocalPassword = get_password(local),
    rebar3_hex_io:say("Generating keys..."),

    WriteKeyEncrypted = encrypt_write_key(Username, LocalPassword, WriteKey),

    %% read key
    RepoConfig1 = Repo#{api_key => WriteKey},
    ReadKeyName = api_key_name("read"),
    ReadPermissions = [#{<<"domain">> => <<"api">>, <<"resource">> => <<"read">>}],
    ReadKey = generate_key(RepoConfig1, ReadKeyName, ReadPermissions),

    %% repo key
    ReposKeyName = repos_key_name(),
    ReposPermissions = [#{<<"domain">> => <<"repositories">>}],
    ReposKey = generate_key(RepoConfig1, ReposKeyName, ReposPermissions),

    % By default a repositories key is created which gives user access to all repositories
    % that they are granted access to server side.
    rebar3_hex_config:update_auth_config(#{RepoName => #{
                                                     username => Username,
                                                     write_key => WriteKeyEncrypted,
                                                     read_key => ReadKey,
                                                     repo_key => ReposKey}}, State),
    rebar3_hex_io:say("You are now ready to interact with your hex repositories."),
    {ok, State};

handle_task(#{args := #{task := deauth}} = Task) ->
    #{repo := Repo, state := State} = Task,
    case Repo of
        #{username := Username, name := RepoName} ->
            rebar3_hex_config:update_auth_config(#{RepoName => #{}}, State),
            rebar3_hex_io:say("User `~s` removed from the local machine. "
                     "To authenticate again, run `rebar3 hex user auth` "
                     "or create a new user with `rebar3 hex user register`", [Username]),
            {ok, State};
        _ ->
            rebar3_hex_io:say("Not authenticated as any user currently for this repository"),
            {ok, State}
    end;

handle_task(#{args := #{task := reset_password, account := true}} = Task) ->
    #{repo := Repo, state := State} = Task,
    User = get_string_input("Username or Email"),
    case rebar3_hex_client:reset_password(Repo, rebar_utils:to_binary(User)) of
        {ok, _} ->
             rebar3_hex_io:say("Email with reset link sent", []),
             {ok, State};
        {error, #{<<"message">> := Message}} ->
            ?RAISE({reset_account_password, Message});
        Error ->
            ?RAISE({reset_account_password, Error})
    end;

%% TODO: Write a test
handle_task(#{args := #{task := reset_password, local := true}} = Task) ->
    #{repo := Repo, state := State} = Task,
    case Repo of
        #{username := Username, write_key := EncryptedWriteKey, read_key := ReadKey, repo_key := ReposKey} ->
            DecryptedWriteKey = decrypt_write_key(Username, EncryptedWriteKey),
            LocalPassword = get_password(new_local),
            NewEncryptedWriteKey = encrypt_write_key(Username, LocalPassword, DecryptedWriteKey),
            rebar3_hex_config:update_auth_config(#{?DEFAULT_HEX_REPO => #{
                                                     username => Username,
                                                     write_key => NewEncryptedWriteKey,
                                                     read_key => ReadKey,
                                                     repo_key => ReposKey}}, State),

            {ok, State};
        _ ->
            rebar3_hex_io:say("Not authenticated as any user currently for this repository"),
            {ok, State}
    end;

handle_task(#{args := #{task := whoami}, state := State}) ->
    Parents = rebar3_hex_config:parent_repos(State),
    [whoami(R, State) || R <- Parents],
    {ok, State};

handle_task(#{args := #{task := key, generate := true} = Args} = Task) ->
    #{raw_opts := Opts, repo := Repo, state := State} = Task,
    KeyName = maps:get(key_name, Args, undefined),
    Username = get_string_input("Username"),
    Password  = get_password(account),
    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    Config = Repo#{api_key => rebar_utils:to_binary("Basic " ++ Auth)},
    PermOpts = proplists:get_all_values(permission, Opts),
    Perms = rebar3_hex_key:convert_permissions(PermOpts, [#{<<"domain">> => <<"api">>}]),
    _ = generate_key(Config, KeyName, Perms),
    rebar3_hex_io:say("Key successfully created", []),
    {ok, State};

handle_task(#{args := #{task := key, revoke := true, all := true}} = Task) ->
    #{repo := Repo, state := State} = Task,
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    case rebar3_hex_key:revoke_all(Config) of
        ok ->
            rebar3_hex_io:say("All keys successfully revoked", []),
            {ok, State};
        Error ->
            ?RAISE({key_revoke_all, Error})
    end;

handle_task(#{args := #{task := key, revoke := true, key_name := KeyName}} = Task) ->
    #{repo := Repo, state := State} = Task,
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, write),
    case rebar3_hex_key:revoke(Config, KeyName) of
        ok ->
            rebar3_hex_io:say("Key successfully revoked", []),
            {ok, State};
        Error ->
            ?RAISE({key_revoke, Error})
    end;

handle_task(#{repo := Repo, state := State, args := #{task := key, list := true}}) ->
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, read),
    case rebar3_hex_key:list(Config) of
         ok ->
            {ok, State};
        Error ->
            ?RAISE({key_list, Error})
    end;

handle_task(#{args := #{task := key, fetch := true, key_name := KeyName}} = Task) ->
    #{repo := Repo, state := State} = Task,
    Config = rebar3_hex_config:get_hex_config(?MODULE, Repo, read),
    case rebar3_hex_key:fetch(Config, KeyName) of
         ok ->
            {ok, State};
        Error ->
            ?RAISE({key_list, Error})
    end;

handle_task(_) ->
    ?RAISE(bad_command).

-dialyzer({no_fail_call, whoami/2}).
whoami(#{name := Name} = Repo, State) ->
    case maps:get(read_key, Repo, undefined) of
        undefined ->
            ?RAISE(not_authenticated);
        ReadKey ->
            case rebar3_hex_client:me(maps:remove(name, Repo#{api_key => ReadKey})) of
                {ok, #{<<"username">> := Username,
                                       <<"email">> := Email}} ->
                    rebar3_hex_io:say("~ts : ~ts (~ts)", [Name, Username, Email]),
                    {ok, State};
                {error, #{<<"message">> := Message}} ->
                    ?RAISE({whoami, Message});
                Err  ->
                    ?RAISE({whoami, Err})
            end
    end.

get_string_input(Prompt) ->
    MaxRetries = 3,
    do_get_string_input(Prompt, MaxRetries).

do_get_string_input(Prompt, 0) ->
    ?RAISE({input_required, Prompt});

do_get_string_input(Prompt, MaxRetries) ->
    case rebar3_hex_io:ask(Prompt ++ ":", string, "") of
        no_data ->
            rebar_api:warn("A ~ts is required for this task, please try again.", [Prompt]),
            do_get_string_input(Prompt, MaxRetries - 1);
        Username ->
            rebar_utils:to_binary(Username)
    end.

local_password_check(Pw) ->
    byte_size(Pw) < 32 orelse "Local passwords can not be greater than 32 characters, please try again".

get_password(account) ->
    get_password(<<"Account">>, fun(_) -> true end);

get_password(new_local) ->
    get_password(<<"New local">>, fun(Pw) -> local_password_check(Pw) end);

get_password(local) ->
    get_password(<<"Local">>, fun(Pw) -> local_password_check(Pw) end).

get_password(Type, Fun) when is_binary(Type) ->
    MaxRetries = 3,
    do_get_password(Type, Fun, MaxRetries).

do_get_password(_, _, 0) ->
    ?RAISE(passwords_do_not_match);

do_get_password(Type, Fun, MaxRetries) ->
    Rest = <<" Password: ">>,
    Password = rebar3_hex_io:get_password(<<Type/binary, Rest/binary>>),
    case Fun(Password) of
        true ->
            confirm_password(Type, Fun, Password, MaxRetries);
        Err ->
            rebar_api:warn(Err, []),
            do_get_password(Type, Fun, MaxRetries - 1)
    end.

confirm_password(Type, Fun, ExpectedPw, MaxRetries) ->
    Rest = <<" Password (confirm): ">>,
    case rebar3_hex_io:get_password(<<Type/binary, Rest/binary>>) of
        Pw when Pw =:= ExpectedPw ->
            Pw;
        _ ->
            rebar_api:warn("Passwords do not match, please try again", []),
            do_get_password(Type, Fun,  MaxRetries - 1)
    end.

-dialyzer({no_fail_call, create_user/5}).
create_user(Username, Email, Password, Repo, State) ->
    case rebar3_hex_client:create_user(Repo, Username, Password, Email) of
        {ok, _} ->
            rebar3_hex_io:say("You are required to confirm your email to access your account, "
                        "a confirmation email has been sent to ~s", [Email]),
            rebar3_hex_io:say("Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
                        [maps:get(repo_name, Repo)]),
            {ok, State};
        {error, #{<<"errors">> := Errors}} ->
            ?RAISE({registration_failure, Errors});
        Error ->
            ?RAISE({registration_failure, Error})
    end.

%% @private
encrypt_write_key(Username, LocalPassword, WriteKey) ->
    rebar3_hex_config:encrypt_write_key(Username, LocalPassword, WriteKey).

%% @private
%-spec decrypt_write_key(binary(), {binary(), {binary(), binary()}} | undefined) -> binary().
decrypt_write_key(_Username, undefined) ->
    {decrypt_write_key, no_write_key};
decrypt_write_key(Username, Key) ->
    MaxRetries = 2,
    LocalPassword = rebar3_hex_io:get_password(<<"Local Password: ">>),
    decrypt_write_key(Username, LocalPassword, Key, MaxRetries).


-ifdef(POST_OTP_22).
decrypt_write_key(_, _, _, 0) ->
    ?RAISE(bad_local_password);

decrypt_write_key(Username, LocalPassword, Key, MaxRetries) ->
    case rebar3_hex_config:decrypt_write_key(Username, LocalPassword, Key) of
        error ->
            rebar_api:warn("Sorry, try again.", []),
            LocalPassword1 = rebar3_hex_io:get_password(<<"Local Password: ">>),
            decrypt_write_key(Username, LocalPassword1, Key, MaxRetries - 1);
        Result ->
            Result
    end.
-else.
decrypt_write_key(_, _, _, 0) ->
    ?RAISE(bad_local_password);

decrypt_write_key(Username, LocalPassword, Key, MaxRetries) ->
    case rebar3_hex_config:decrypt_write_key(Username, LocalPassword, Key) of
        error ->
            rebar_api:warn("Sorry, try again.", []),
            LocalPassword1 = rebar3_hex_io:get_password(<<"Local Password: ">>),
            decrypt_write_key(Username, LocalPassword1, Key, MaxRetries - 1);
        Result ->
            Result
    end.
-endif.

-dialyzer({nowarn_function, generate_key/3}).
%%-dialyzer({nowarn_function, hex_api_key:add/3}).
generate_key(HexConfig, KeyName, Perms) ->
    case rebar3_hex_key:generate(HexConfig, KeyName, Perms) of
        {ok, #{<<"secret">> := Secret}} ->
            Secret;
        {error, #{<<"message">> := Message}} ->
            ?RAISE({generate_key, Message});
        Error ->
            ?RAISE({generate_key, Error})
    end.

hostname() ->
    {ok, Name} = inet:gethostname(),
    Name.

api_key_name() ->
    rebar_utils:to_binary(hostname()).

-dialyzer({nowarn_function, api_key_name/1}).
api_key_name(Postfix) ->
    rebar_utils:to_binary([hostname(), "-api-", Postfix]).

-dialyzer({nowarn_function, repos_key_name/0}).
repos_key_name() ->
    rebar_utils:to_binary([hostname(), "-repositories"]).


