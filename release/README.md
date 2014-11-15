# To make a release

```
$ mkdir -p rel
$ erl
>  {ok, Conf} = file:consult("erlbot-1.0.config").
>  {ok, Spec} = reltool:get_target_spec(Conf).
> reltool:eval_target_spec(Spec, code:root_dir(), "rel").
```

