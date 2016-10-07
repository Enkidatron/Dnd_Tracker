defmodule DndTracker.StatBlockControllerTest do
  use DndTracker.ConnCase

  alias DndTracker.StatBlock
  @valid_attrs %{bonusHealth: 42, dieFace: 42, health: 42, initiative: 42, name: "some content", numDie: 42, useHitDie: true, user: "some content"}
  @invalid_attrs %{}

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  test "lists all entries on index", %{conn: conn} do
    conn = get conn, stat_block_path(conn, :index)
    assert json_response(conn, 200)["data"] == []
  end

  test "shows chosen resource", %{conn: conn} do
    stat_block = Repo.insert! %StatBlock{}
    conn = get conn, stat_block_path(conn, :show, stat_block)
    assert json_response(conn, 200)["data"] == %{"id" => stat_block.id,
      "user" => stat_block.user,
      "name" => stat_block.name,
      "initiative" => stat_block.initiative,
      "health" => stat_block.health,
      "numDie" => stat_block.numDie,
      "dieFace" => stat_block.dieFace,
      "bonusHealth" => stat_block.bonusHealth,
      "useHitDie" => stat_block.useHitDie}
  end

  test "renders page not found when id is nonexistent", %{conn: conn} do
    assert_error_sent 404, fn ->
      get conn, stat_block_path(conn, :show, -1)
    end
  end

  test "creates and renders resource when data is valid", %{conn: conn} do
    conn = post conn, stat_block_path(conn, :create), stat_block: @valid_attrs
    assert json_response(conn, 201)["data"]["id"]
    assert Repo.get_by(StatBlock, @valid_attrs)
  end

  test "does not create resource and renders errors when data is invalid", %{conn: conn} do
    conn = post conn, stat_block_path(conn, :create), stat_block: @invalid_attrs
    assert json_response(conn, 422)["errors"] != %{}
  end

  test "updates and renders chosen resource when data is valid", %{conn: conn} do
    stat_block = Repo.insert! %StatBlock{}
    conn = put conn, stat_block_path(conn, :update, stat_block), stat_block: @valid_attrs
    assert json_response(conn, 200)["data"]["id"]
    assert Repo.get_by(StatBlock, @valid_attrs)
  end

  test "does not update chosen resource and renders errors when data is invalid", %{conn: conn} do
    stat_block = Repo.insert! %StatBlock{}
    conn = put conn, stat_block_path(conn, :update, stat_block), stat_block: @invalid_attrs
    assert json_response(conn, 422)["errors"] != %{}
  end

  test "deletes chosen resource", %{conn: conn} do
    stat_block = Repo.insert! %StatBlock{}
    conn = delete conn, stat_block_path(conn, :delete, stat_block)
    assert response(conn, 204)
    refute Repo.get(StatBlock, stat_block.id)
  end
end
