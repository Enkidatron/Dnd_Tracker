defmodule DndTracker.StatBlock do
  use DndTracker.Web, :model

  schema "stat_blocks" do
    field :user, :string
    field :name, :string
    field :initiative, :integer
    field :health, :integer
    field :numDie, :integer
    field :dieFace, :integer
    field :bonusHealth, :integer
    field :useHitDie, :boolean, default: false

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:user, :name, :initiative, :health, :numDie, :dieFace, :bonusHealth, :useHitDie])
    |> validate_required([:user, :name, :initiative, :health, :numDie, :dieFace, :bonusHealth, :useHitDie])
  end
end
