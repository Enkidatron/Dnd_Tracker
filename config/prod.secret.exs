use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
#
# You should document the content of this
# file or create a script for recreating it, since it's
# kept out of version control and might be hard to recover
# or recreate for your teammates (or you later on).
config :dnd_tracker, DndTracker.Endpoint,
  secret_key_base: "CpHtmufvVN4OlH9Da4i9gXSdUryuUNIwW92mOLU02ormbILr9HRKFpKip6OS9Dxz"

# Configure your database
config :dnd_tracker, DndTracker.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "dnd_tracker_prod",
  pool_size: 20
