{application, erlbot,
    [{vsn, "1.0.0"},
     {description, "A silly little IRC bot"},
     {modules, [erlbot_app,erlbot_sup,erlbot_callbacks]},
     {registered, []},
     {applications, [stdlib, kernel]},
     {mod, {erlbot_app, []}}
    ]}.
