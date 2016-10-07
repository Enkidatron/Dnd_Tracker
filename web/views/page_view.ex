defmodule DndTracker.PageView do
  use DndTracker.Web, :view

  def getUrl do
  	env = Application.get_env(:dnd_tracker, DndTracker.Endpoint)
  	getScheme(env[:url]) <> "://" <> getHost(env[:url]) <> ":" <> getPort(env[:url]) <> "/api"
  end

  defp getScheme(%{scheme: scheme}) do
  	scheme
  end
  defp getScheme(_) do
  	"http"
  end

  defp getHost(%{host: host}) do
  	host
  end
  defp getHost(_) do
  	"localhost"
  end

  defp getPort(%{port: port}) when is_integer(port) do
  	Integer.to_string(port)
  end
  defp getPort(%{port: port}) do
  	port
  end
  defp getPort(_) do
  	"4000"
  end

end
