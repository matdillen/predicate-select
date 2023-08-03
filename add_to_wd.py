import pywikibot

#testvalues
item_id = "Q2115205"
property_id = "P11146"
target_id = "Q15820827"
reference_string = "HTTP://LAGU"

#def remove_reference(item_id, property_id, target_id, reference_string):
site = pywikibot.Site("wikidata", "wikidata")
repo = site.data_repository()
site.login()

# Get the item that has the statement with the specified property and target
item = pywikibot.ItemPage(repo, item_id)
statement = pywikibot.Claim(repo, property_id)
statement.setTarget(pywikibot.ItemPage(repo, target_id))

# Load the item to access the statements
#item.get()

if statement.getID() in item.get().get("claims"):
    for claim in item.get().get("claims")[statement.getID()]:
        if "references" in claim.toJSON():
            # Filter and remove the specified reference
            new_references = []
            do_edit = False
            for reference in claim.sources:
                keep_reference = True
                for refclaim in reference.values():
                    refclaim_j = refclaim[0].toJSON()
                    if refclaim_j.get("snaktype") == "value" and refclaim_j.get("datavalue").get("value") == reference_string:
                        keep_reference = False
                        do_edit = True
                        new_references.extend(refclaim)
                        break
                if keep_reference:
                    break
            if do_edit:
                claim.removeSource(new_references[0], summary = "Remove reference with incorrect IH code")
                print("remove:\n")
                print(new_references)
                # for reference in new_references:
                    # print("\nadd:\n")
                    # print([reference])
                    # claim.addSources([reference], summary="Removing specified reference")
                # print("Reference removed successfully.")
                #return

#print("Statement or reference not found.")


# Example usage


#remove_reference(item_id, property_id, target_id, reference_string)