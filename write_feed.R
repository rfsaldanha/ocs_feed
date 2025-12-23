write_feed <- function(
  entries,
  atom_ns = "http://www.w3.org/2005/Atom",
  feed_url = "example.org",
  site_url = "https://climaesaude.icict.fiocruz.br",
  subtitle = "Observatório de Clima e Saúde - LIS/ICICT/Fiocruz"
) {
  # Create the root node <feed> with the required namespace
  feed <- xml2::xml_new_root("feed", xmlns = atom_ns)

  # Add required feed-level elements
  xml2::xml_add_child(feed, "title", paste("Alerta", entries[[1]]$title))
  xml2::xml_add_child(feed, "subtitle", subtitle)
  xml2::xml_add_child(feed, "link", href = site_url)
  xml2::xml_add_child(feed, "link", href = feed_url, rel = "self")
  xml2::xml_add_child(feed, "id", site_url)
  xml2::xml_add_child(feed, "updated", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))

  # Add an entry (post) to the feed
  for (i in 1:length(entries)) {
    entry <- xml2::xml_add_child(feed, "entry")
    xml2::xml_add_child(entry, "title", entries[[i]]$title)
    xml2::xml_add_child(entry, "content", entries[[i]]$message)
    # xml2::xml_add_child(entry, "id", uuid::UUIDgenerate())
    xml2::xml_add_child(entry, "id", entries[[i]]$identifier)
  }

  # Write the XML document to a file
  xml2::write_xml(
    feed,
    file = paste0(
      "/dados/home/rfsaldanha/ocs_feed/feeds/",
      entries[[1]]$feed,
      ".xml"
    )
  )
}
