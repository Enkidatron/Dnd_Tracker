defmodule DndTracker.Router do
  use DndTracker.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    # plug Plug.Parsers, parsers: [:json], json_decoder: Poison
    # Can we parse the incoming JSON body here?
  end

  scope "/", DndTracker do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/login", PageController, :login
  end

  # Other scopes may use custom stacks.
  scope "/api", DndTracker do
    pipe_through :api

    resources "/stat_blocks", StatBlockController, except: [:new, :edit]
  end

  scope "/auth", DndTracker do
    pipe_through :browser # Use the default browser stack

    get "/:provider", AuthController, :index
    get "/:provider/callback", AuthController, :callback
    delete "/logout", AuthController, :delete
  end
end
