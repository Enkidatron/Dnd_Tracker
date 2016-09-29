defmodule DndTracker.PageController do
  use DndTracker.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
