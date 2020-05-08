# Pacchettibotti

![Status badge](https://healthchecks.io/badge/554e4132-70f8-437a-8efe-d69189/uQG8RD2w.svg)

This repo is the software that powers [Pacchettibotti][pacchettibotti], a bot
that takes care of automating updates across various repositories belonging to
PureScript's package infrastructure.

## Responsibilities

- Keep the package versions in [package-sets] up to date as new versions of
  the packages in the set are released.
  Enabled by the `enablePackageSetsUpdate` setting.
  Requires write access to [package-sets].
- Keep the [registry] in sync with new versions of packages released on the
  Bower registry.
  Enabled by the `enableRegistryBowerSync` setting.
  Requires write access to the [registry].
- Keep the [package-sets-metadata repo][package-sets-metadata] up to date.
  Enabled by the `enableSetsMetadataUpdate` setting.
  Requires write access to said repo.
- Keep [purescript] version up to date in Spago CI scripts.
  Enabled by the `enableVersionUpdates` setting.
  Requires write access to [spago]
- Keep [package-sets] version up to date in Spago templates.
  Enabled by the `enableVersionUpdates` setting.
  Requires write access to [spago]
- Keep [docs-search] version up to date in Spago.
  Enabled by the `enableVersionUpdates` setting.
  Requires write access to [spago]

## Current deployment

The deployment of the bot is currently maintained by [**@f-f**][f-f], and you
can see its status (i.e. up/down) through the badge at the top.

## Internals

TODO: document DB: why, how, what

## Developing

### Dev environment

Using [stack] is the recommended option unless you know what you're doing.

Common workflows:
- `stack build` to compile the project
- `stack install` to copy the executable to your path

### Runtime requirements

The project compiles to an executable called `pacchettibotti`.

You should:
- run it into [`nix-shell`][nix] (see [`launch.sh`](./launch.sh))
- have a [`config.json`](./config.json)
- define the `PACCHETTIBOTTI_GITHUB_TOKEN` environment variable, containing a
  [GitHub token][gh-tokens].  
  Note: you should not need to add any authorization, as the token will only be
  used to open Pull Requests.  
  Note: right now one is supposed to have write access to all the repos (as
  [**@pacchettibotti**][pacchettibotti] has), and this is not
  very nice.
  In order to enable easy testing of the changes and/or being able to run this on
  any other account we should make this generic - this is tracked in [#3][make-repo-generic]


[nix]: https://nixos.org/nix/
[f-f]: https://github.com/f-f
[spago]: https://github.com/purescript/spago
[stack]: https://docs.haskellstack.org/en/stable/README/
[registry]: https://github.com/purescript/registry
[gh-tokens]: https://github.com/settings/tokens
[purescript]: https://github.com/purescript/purescript
[docs-search]: https://github.com/spacchetti/purescript-docs-search
[package-sets]: https://github.com/purescript/package-sets
[pacchettibotti]: https://github.com/pacchettibotti
[make-repo-generic]: https://github.com/spacchetti/pacchettibotti/issues/3
[package-sets-metadata]: https://github.com/spacchetti/package-sets-metadata