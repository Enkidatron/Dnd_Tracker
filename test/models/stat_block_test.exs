defmodule DndTracker.StatBlockTest do
  use DndTracker.ModelCase

  alias DndTracker.StatBlock

  @valid_attrs %{bonusHealth: 42, dieFace: 42, health: 42, initiative: 42, name: "some content", numDie: 42, useHitDie: true, user: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = StatBlock.changeset(%StatBlock{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = StatBlock.changeset(%StatBlock{}, @invalid_attrs)
    refute changeset.valid?
  end
end
