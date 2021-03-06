defmodule DndTracker.AuthController do
	use DndTracker.Web, :controller

	def index(conn, %{"provider" => provider}) do
		redirect conn, external: authorize_url!(provider)
	end

	def callback(conn, %{"provider" => provider, "code" => code}) do
		token = get_token!(provider, code)
		user = get_user!(provider, token)

		conn 
		|> put_session(:current_user, user)
		|> put_session(:access_token, token.token.access_token)
		|> redirect(to: "/")
	end

	def delete(conn, _params) do
		conn
		|> put_flash(:info, "You have been logged out!")
		|> configure_session(drop: true)
		|> redirect(to: "/login")
	end


	defp authorize_url!("google") do
		Google.authorize_url!(scope: "email profile")
	end
	defp authorize_url!(_) do
		raise "No matching provider available"
	end

	defp get_token!("google", code) do
		Google.get_token!(code: code)
	end
	defp get_token!(_,_) do
		raise "No matching provider available"
	end

	defp get_user!("google", token) do
		{:ok, %{body: user}} = OAuth2.Client.get(token, "https://www.googleapis.com/plus/v1/people/me/openIdConnect")
		%{name: user["name"], id: user["sub"]}
	end
end