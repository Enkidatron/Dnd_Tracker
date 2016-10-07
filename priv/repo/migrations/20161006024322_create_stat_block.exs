defmodule DndTracker.Repo.Migrations.CreateStatBlock do
  use Ecto.Migration

  def change do
    create table(:stat_blocks) do
      add :user, :string
      add :name, :string
      add :initiative, :integer
      add :health, :integer
      add :numDie, :integer
      add :dieFace, :integer
      add :bonusHealth, :integer
      add :useHitDie, :boolean, default: false, null: false

      timestamps()
    end

    create index(:stat_blocks, [:user])

  end
end
