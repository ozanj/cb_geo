-- Single-space the figure-note blocks in docx output and drop the bullets.
-- Each note is a "Figure notes:" paragraph followed by a one-item list.
-- We flatten that list into plain paragraphs (no bullet) and wrap the whole
-- block in the single-spaced custom "FigureNotes" paragraph style. Flattening
-- is required because a custom-style div does NOT propagate to list items, so
-- bulleted notes kept the double-spaced Compact style.

if not FORMAT:match("docx") then
  return {}
end

local function is_fig_notes(blk)
  return blk.t == "Para" and pandoc.utils.stringify(blk):match("^%s*Figure notes")
end

function Blocks(blocks)
  local out = pandoc.List()
  local i = 1
  while i <= #blocks do
    local b = blocks[i]
    if is_fig_notes(b) then
      local grp = pandoc.List()
      grp:insert(b) -- the "Figure notes:" intro paragraph
      -- flatten the following list into plain paragraphs (removes the bullets)
      if i + 1 <= #blocks and (blocks[i + 1].t == "BulletList" or blocks[i + 1].t == "OrderedList") then
        for _, item in ipairs(blocks[i + 1].content) do
          for _, item_blk in ipairs(item) do
            grp:insert(item_blk)
          end
        end
        i = i + 1
      end
      out:insert(pandoc.Div(grp, pandoc.Attr("", {}, { ["custom-style"] = "FigureNotes" })))
    else
      out:insert(b)
    end
    i = i + 1
  end
  return out
end
