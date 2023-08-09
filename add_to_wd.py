import pywikibot
import pandas as pd

def remove_reference(item_id, property_id, target_id, reference_string,site,repo):
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
                    resp = claim.on_item.repo.removeSources(claim,new_references, summary = "Remove reference with incorrect IH code")
                    claim.on_item.latest_revision_id = resp['pageinfo']['lastrevid']
                    print("remove:\n")
                    print(new_references)

# Example usage

data = pd.read_table("quickstatements_broken_jacqid.txt")

site = pywikibot.Site("wikidata", "wikidata")
repo = site.data_repository()
site.login()

for i,r in data.iloc[7:].iterrows():
    ref_string = "HTTP://" + r['jacq_id']
    remove_reference(r['recordedBy_IRI'], "P11146", r['qid'], ref_string,site,repo)