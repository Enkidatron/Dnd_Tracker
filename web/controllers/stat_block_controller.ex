defmodule DndTracker.StatBlockController do
  use DndTracker.Web, :controller

  alias DndTracker.StatBlock

  def index(conn, _params) do
    stat_blocks = Repo.all(StatBlock)
    render(conn, "index.json", stat_blocks: stat_blocks)
  end

  def create(conn, %{"stat_block" => stat_block_params}) do
    # I'd really rather not have to manually parse this
    stat_block_params = Poison.Parser.parse!(stat_block_params)
    current_user = get_session(conn, :current_user)
    stat_block_params = Map.put(stat_block_params, "user", current_user.id)
    IO.puts inspect(stat_block_params)
    changeset = StatBlock.changeset(%StatBlock{}, stat_block_params)
    case Repo.insert(changeset) do
      {:ok, stat_block} ->
        conn
        |> put_status(:created)
        |> put_resp_header("location", stat_block_path(conn, :show, stat_block))
        |> render("show.json", stat_block: stat_block)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(DndTracker.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    stat_block = Repo.get!(StatBlock, id)
    render(conn, "show.json", stat_block: stat_block)
  end

  def update(conn, %{"id" => id, "stat_block" => stat_block_params}) do
    stat_block = Repo.get!(StatBlock, id)
    changeset = StatBlock.changeset(stat_block, stat_block_params)

    case Repo.update(changeset) do
      {:ok, stat_block} ->
        render(conn, "show.json", stat_block: stat_block)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(DndTracker.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    stat_block = Repo.get!(StatBlock, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(stat_block)

    send_resp(conn, :no_content, "")
  end
end
