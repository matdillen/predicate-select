import pywikibot

def remove_reference(item_id, property_id, target_id, reference_string):
site = pywikibot.Site("wikidata", "wikidata")
repo = site.data_repository()

# Get the item that has the statement with the specified property and target
item = pywikibot.ItemPage(repo, item_id)
statement = pywikibot.Claim(repo, property_id)
statement.setTarget(pywikibot.ItemPage(repo, target_id))

# Load the item to access the statements
item.get()

if statement.getID() in item.get().get("claims"):
    for claim in item.get().get("claims")[statement.getID()]:
        if "references" in claim.toJSON():
            # Filter and remove the specified reference
            new_references = []
            do_edit = False
            for reference in claim.toJSON()["references"]:
                keep_reference = True
                test = reference
                for snaks in reference["snaks"]:
                    for snak in reference["snaks"][snaks]:
                        if snak.get("snaktype") == "value" and snak.get("datavalue").get("value") == reference_string:
                            keep_reference = False
                            do_edit = True
                            break
                    if not keep_reference:
                        break
                if keep_reference:
                    new_references.append(reference)
            if do_edit:
                claim.removeSources(claim.sources)
                print("remove:\n")
                print(claim.sources)
                for reference in new_references:
                    print("\nadd:\n")
                    print([reference])
                    claim.addSources([reference], summary="Removing specified reference")

                print("Reference removed successfully.")
                return

print("Statement or reference not found.")


# Example usage
item_id = "Q1333160"
property_id = "P11146"
target_id = "Q15820827"
reference_string = "HTTP://LAGU"

remove_reference(item_id, property_id, target_id, reference_string)