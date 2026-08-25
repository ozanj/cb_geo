-- Convert footnotes to endnotes for docx output.
-- Collects every footnote, replaces it in-text with a superscript number,
-- and drops the collected notes into the "::: {#endnotes} :::" placeholder
-- div (falls back to the end of the document if that div is absent).
-- Gated to docx so PDF output still uses the LaTeX `endnotes` package.

if not FORMAT:match("docx") then
  return {}
end

local notes = pandoc.List()
local placed = false

-- Emit endnotes as numbered *plain paragraphs* (not an ordered list). A pandoc
-- ordered list renders as Word list items with no paragraph style (-> Normal,
-- single-spaced); plain paragraphs get the Body Text style, which is double-spaced.
local function endnote_blocks()
  local blocks = pandoc.List()
  blocks:insert(pandoc.Header(1, pandoc.Str("Endnotes"), pandoc.Attr("", { "unnumbered" })))
  local entries = pandoc.List()
  for i, note in ipairs(notes) do
    local nb = note
    local first = nb[1]
    local prefix = pandoc.List({ pandoc.Str(tostring(i) .. "."), pandoc.Space() })
    if first and (first.t == "Para" or first.t == "Plain") then
      local inl = pandoc.List()
      inl:extend(prefix)
      inl:extend(first.content)
      first.content = inl
    else
      table.insert(nb, 1, pandoc.Para(prefix))
    end
    for _, b in ipairs(nb) do
      entries:insert(b)
    end
  end
  -- Flush (non-indented), double-spaced style, so endnotes don't inherit the
  -- body's first-line indent. Custom-style applies to these plain paragraphs.
  blocks:insert(pandoc.Div(entries, pandoc.Attr("", {}, { ["custom-style"] = "EndnotesEntry" })))
  return blocks
end

-- Collected in document order, so numbering matches appearance.
function Note(el)
  notes:insert(el.content)
  return pandoc.Superscript(pandoc.Str(tostring(#notes)))
end

-- Fill the #endnotes placeholder once every footnote has been collected.
-- (The div sits after all footnote references, so notes is complete here.)
function Div(el)
  if el.identifier == "endnotes" and #notes > 0 then
    placed = true
    return endnote_blocks()
  end
end

-- Fallback: no #endnotes div in the document -> append at the very end.
function Pandoc(doc)
  if #notes == 0 or placed then
    return doc
  end
  doc.blocks:extend(endnote_blocks())
  return doc
end
