package modules.admin.Audit.models;

import modules.admin.domain.Audit;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.ComparisonComposite;
import org.skyve.metadata.view.model.ComparisonComposite.Mutation;
import org.skyve.metadata.view.model.ComparisonModel;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.util.JSONUtil;

public class AuditComparisonModel implements ComparisonModel<Audit> {
	private static final long serialVersionUID = 5964879680504956032L;

	@Override
	public ComparisonComposite getComparisonComposite(Audit audit) throws Exception {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(Audit.MODULE_NAME);
		Document d = m.getDocument(c, Audit.DOCUMENT_NAME);
		
		PersistentBean current = CORE.getPersistence().retrieve(d, audit.getAuditBizId(), false);
		
//		System.out.println(JSONUtil.unmarshall(u, audit.getAudit()));

		ComparisonComposite result = new ComparisonComposite(audit.getBizId(),
																current.getBizKey(),
																null,
																Mutation.unchanged,
																d.getSingularAlias());

/*
		ComparisonProperty urlProperty = new ComparisonProperty(returnDoc.getAttribute(GrowerReturn.urlPropertyName), oldGrowerReturn, newGrowerReturn);
		result.getProperties().add(urlProperty);
		
		// Add any added or updated vineyards
		for (Vineyard newVineyard : newGrowerReturn.getVineyards()) {
			// Find old vineyard
			Integer vineyardNo = newVineyard.getVineyardNo();
			Vineyard oldVineyard = null;
			for (Vineyard vineyard : oldGrowerReturn.getVineyards()) {
				if (vineyardNo.equals(vineyard.getVineyardNo())) {
					oldVineyard = vineyard;
					break;
				}
			}
			if (oldVineyard != null) {
				oldGrowerReturn.getVineyards().remove(oldVineyard);
			}

			result.getChildren().add(createVineyard(vineyardDoc,
														existingPlantingDoc,
														newPlantingDoc,
														oldVineyard,
														newVineyard));
		}

		// Add any deleted vineyards
		for (Vineyard deletedVineyard : oldGrowerReturn.getVineyards()) {
			result.getChildren().add(createVineyard(vineyardDoc,
														existingPlantingDoc,
														newPlantingDoc,
														deletedVineyard,
														null));
		}
*/		
		return result;
	}
/*
	private static ComparisonComposite createVineyard(Document vineyardDoc,
														Document existingPlantingDoc,
														Document newPlantingDoc,
														Vineyard oldVineyard,
														Vineyard newVineyard)
	throws Exception {
		ComparisonComposite result = null;
		if (newVineyard != null) { // added or updated
			result = new ComparisonComposite(newVineyard.getBizId(), 
												newVineyard.getBizKey(),
												GrowerReturn.vineyardsPropertyName,
												(oldVineyard == null) ? Mutation.added : Mutation.unchanged,
												"Vineyard");
		}
		else {
			result = new ComparisonComposite(oldVineyard.getBizId(), 
												oldVineyard.getBizKey(),
												GrowerReturn.vineyardsPropertyName,
												Mutation.deleted,
												"Vineyard");
		}
		List<ComparisonProperty> properties = result.getProperties();
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.vineyardNoPropertyName), oldVineyard, newVineyard));
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.namePropertyName), oldVineyard, newVineyard));
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.regionPropertyName), oldVineyard, newVineyard));
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.urlPropertyName), oldVineyard, newVineyard));
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.existingPlantingsCommentPropertyName), oldVineyard, newVineyard));
		properties.add(new ComparisonProperty(vineyardDoc.getAttribute(Vineyard.newPlantingsCommentPropertyName), oldVineyard, newVineyard));
		
		// we will remove processed ones from this list, not the original list
		List<ExistingPlanting> newPlantingsClone = null;
		if (newVineyard == null) {
			newPlantingsClone = new ArrayList<ExistingPlanting>(0);
		}
		else {
			newPlantingsClone = new ArrayList<ExistingPlanting>(newVineyard.getExistingPlantings());
		}
		
		if (oldVineyard != null) {
			for (ExistingPlanting oldPlanting : oldVineyard.getExistingPlantings()) {
				String vineyardPlantingId = oldPlanting.getVineyardPlantingBizId();
				ExistingPlanting newPlanting = null;
				for (ExistingPlanting planting : newPlantingsClone) {
					if (vineyardPlantingId.equals(planting.getVineyardPlantingBizId())) {
						newPlanting = planting;
						break;
					}
				}
				if (newPlanting != null) {
					newPlantingsClone.remove(newPlanting);
				}
	
				result.getChildren().add(createExistingPlanting(existingPlantingDoc, oldPlanting, newPlanting));
			}
		}
		
		// Add any added plantings
		for (ExistingPlanting addedPlanting : newPlantingsClone) {
			result.getChildren().add(createExistingPlanting(existingPlantingDoc, null, addedPlanting));
		}

		if (newVineyard != null) {
			for (NewPlanting newPlanting : newVineyard.getNewPlantings()) {
				result.getChildren().add(createNewPlanting(newPlantingDoc, newPlanting));
			}
		}
		
		return result;
	}
	
	private static ComparisonComposite createExistingPlanting(Document existingPlantingDoc,
																ExistingPlanting oldPlanting,
																ExistingPlanting newPlanting)
	throws Exception {
		ComparisonComposite result = null;
		if (newPlanting != null) { // added or updated
			result = new ComparisonComposite(newPlanting.getBizId(), 
												(oldPlanting == null) ? newPlanting.getBizKey() : oldPlanting.getSerialNo().toString(),
												Vineyard.existingPlantingsPropertyName,
												(oldPlanting == null) ? Mutation.added : Mutation.unchanged,
												"Existing Planting");
		}
		else {
			result = new ComparisonComposite(oldPlanting.getBizId(), 
												oldPlanting.getSerialNo().toString(),
												Vineyard.existingPlantingsPropertyName,
												Mutation.deleted,
												"Existing Planting");
		}
		List<ComparisonProperty> properties = result.getProperties();
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.varietyNamePropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.blockIdPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.areaInHectaresPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.yearPlantedPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.rootstockPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.statusPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.returnCommentPropertyName), oldPlanting, newPlanting));
		properties.add(new ComparisonProperty(existingPlantingDoc.getAttribute(ExistingPlanting.parcelIdPropertyName), oldPlanting, newPlanting));
		return result;
	}
	
	private static ComparisonComposite createNewPlanting(Document newPlantingDoc,
															NewPlanting newPlanting)
	throws Exception {
		ComparisonComposite result = new ComparisonComposite(newPlanting.getBizId(), 
																newPlanting.getBizKey(),
																Vineyard.newPlantingsPropertyName,
																Mutation.added,
																"New Planting");
		List<ComparisonProperty> properties = result.getProperties();
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.varietyPropertyName), null, newPlanting));
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.blockIdPropertyName), null, newPlanting));
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.areaInHectaresPropertyName), null, newPlanting));
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.yearPlantedPropertyName), null, newPlanting));
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.rootstockPropertyName), null, newPlanting));
		properties.add(new ComparisonProperty(newPlantingDoc.getAttribute(NewPlanting.parcelIdPropertyName), null, newPlanting));
		
		return result;
	}
*/
}
