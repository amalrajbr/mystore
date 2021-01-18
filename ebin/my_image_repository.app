{application, my_image_repository,
 [{description, "My Image Repository application"},
  {vsn, "1.0.0"},
  {modules, [
	     my_image_repository_app,
	     my_image_repository_sup
            ]},
  {registered, [
		my_image_repository_sup
	       ]},
  {applications, [kernel, stdlib]},
  {env,[]},
  {mod, {my_image_repository_app, []}}]}.
