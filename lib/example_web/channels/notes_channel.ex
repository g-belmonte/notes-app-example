defmodule ExampleWeb.NotesChannel do
  use ExampleWeb, :channel

  @impl true
  def join("notes:lobby", payload, socket) do
    if authorized?(payload) do
      {:ok, "Joined lobby", socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
