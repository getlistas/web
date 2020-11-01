# Doneq

## Installation

First, clone the repository:

```sh
git clone git@github.com:gillchristian/doneq.git
cd doneq
```

You can enter a development shell with all non-JavaScript dependencies via Nix:

```sh
nix-shell
```

> Alternately, you can install PureScript, Spago, and Zephyr manually. You can
> use Yarn to install PureScript and Spago, and you can install the Zephyr
> binary [from its releases page](https://github.com/coot/zephyr/releases) --
> ensure it exists in your PATH by moving it to `usr/bin/local`.

Next, install JavaScript dependencies:

```sh
yarn
```

## Building and running

Next, build the project (this command will run `spago build`; see the
[`package.json`](package.json) file to see all helper scripts for the project):

```sh
yarn build
```

You can bundle the JS for production:

```sh
yarn bundle
```

And, once bundled, you can run a local server to use Doneq (defaults to
[port 8080](http://127.0.0.1:8080), but if this port is already in use it will
increment to 8081, etc.):

```sh
yarn serve
```

## Dev mode

To run it in dev mode where saving your changes rebuilds and reloads the app,
you can run the command below (which calls `spago build --watch`) 

```sh
yarn watch
```

And run the application with

```sh
yarn serve-dev
```
This will open your default browser at [port 1234](http://localhost:1234)
