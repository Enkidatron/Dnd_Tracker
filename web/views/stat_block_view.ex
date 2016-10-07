defmodule DndTracker.StatBlockView do
  use DndTracker.Web, :view

  def render("index.json", %{stat_blocks: stat_blocks}) do
    %{data: render_many(stat_blocks, DndTracker.StatBlockView, "stat_block.json")}
  end

  def render("show.json", %{stat_block: stat_block}) do
    %{data: render_one(stat_block, DndTracker.StatBlockView, "stat_block.json")}
  end

  def render("stat_block.json", %{stat_block: stat_block}) do
    %{id: stat_block.id,
      user: stat_block.user,
      name: stat_block.name,
      initiative: stat_block.initiative,
      health: stat_block.health,
      numDie: stat_block.numDie,
      dieFace: stat_block.dieFace,
      bonusHealth: stat_block.bonusHealth,
      useHitDie: stat_block.useHitDie}
  end
end
