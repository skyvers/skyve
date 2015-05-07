package modules.admin.Audit.models;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import modules.admin.domain.Audit;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.ComparisonComposite;
import org.skyve.metadata.view.model.ComparisonComposite.Mutation;
import org.skyve.metadata.view.model.ComparisonModel;
import org.skyve.metadata.view.model.ComparisonProperty;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.model.document.field.Enumeration;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.util.BeanVisitor;
import org.skyve.wildcat.util.JSONUtil;

public class AuditComparisonModel implements ComparisonModel<Audit> {
	private static final long serialVersionUID = 5964879680504956032L;

	@Override
	public ComparisonComposite getComparisonComposite(Audit audit) throws Exception {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		
		Module am = c.getModule(audit.getAuditModuleName());
		Document ad = am.getDocument(c, audit.getAuditDocumentName());
		PersistentBean current = CORE.getPersistence().retrieve(ad, audit.getAuditBizId(), false);

		final Map<String, ComparisonComposite> bindingToNodes = new LinkedHashMap<>();
		
		// Visit the current bean and add in the model structure
		new BeanVisitor() {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Reference owningReference,
										Bean bean,
										boolean visitingInheritedDocument)
			throws Exception {
System.out.println("OB = " + binding + " -> " + bean.getBizId());
				bindingToNodes.put(binding, createNode(owningReference, currentDocument, bean, true));

				return true;
			}
		}.visit(ad, current, c);

		// Visit oldBean and add/modify the resulting model.
		@SuppressWarnings("unchecked")
		Map<String, Object> old = (Map<String, Object>) JSONUtil.unmarshall(u, audit.getAudit());
System.out.println(old);
		for (String binding : old.keySet()) {
			ComparisonComposite node = bindingToNodes.get(binding);
			@SuppressWarnings("unchecked")
			Map<String, Object> oldValues = (Map<String, Object>) old.get(binding);
			// null when the node exists in the new version but not in the old version
			if (oldValues == null) {
				node.setMutation(Mutation.added);
			}
			else {
				boolean nodeDirty = false;

				List<ComparisonProperty> properties = node.getProperties();
				for (ComparisonProperty property : properties) {
					Object oldValue = oldValues.remove(property.getName());
					Attribute attribute = node.getDocument().getAttribute(property.getName());
					Class<?> type = null;
					if (attribute instanceof Enumeration) {
						type = AbstractRepository.get().getEnum((Enumeration) attribute);
					}
					else {
						type = attribute.getAttributeType().getImplementingType();
					}

					if (oldValue instanceof String) {
						oldValue = BindUtil.fromString(c, null, type, (String) oldValue, true);
					}
					else {
						oldValue = BindUtil.convert(type, oldValue);
					}
					property.setOldValue(oldValue);
					if ((! nodeDirty) && property.isDirty()) {
						nodeDirty = true;
					}
				}

				// Process any extra old properties not present in the new version
				for (String name : oldValues.keySet()) {
					ComparisonProperty property = new ComparisonProperty();
					property.setName(name);
					property.setTitle(name);
					property.setOldValue(oldValues.get(name));
					properties.add(property);
					nodeDirty = true;
				}
				node.setMutation(nodeDirty ? Mutation.updated : Mutation.unchanged);
			}
		}

		// need to process any extra nodes in the old data
/*
		new BeanVisitor() {
			@Override
			@SuppressWarnings("synthetic-access")
			protected boolean accept(String binding,
										Document currentDocument,
										Document owningDocument,
										Reference owningReference,
										Bean bean,
										boolean visitingInheritedDocument)
			throws Exception {
				ComparisonComposite node = bindingToNodes.get(binding);
				if (node == null) { // deleted entry
					bindingToNodes.put(binding, createNode(owningReference, currentDocument, bean, false));
				}
				else { // existing entry
					updateNode(bean, node);
				}

				return true;
			}
		}.visit(ad, old, c);
*/
		ComparisonComposite result = null;
		for (String binding : bindingToNodes.keySet()) {
			ComparisonComposite node = bindingToNodes.get(binding);
			if ("".equals(binding)) {
				result = node;
			}
			else {
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex > 0) {
					String parentBinding = binding.substring(0, lastDotIndex);
					bindingToNodes.get(parentBinding).getChildren().add(node);
				}
				else {
					if (result != null) {
						result.getChildren().add(node);
					}
				}
			}
		}
		
		return result;
	}
/*	
	private static void visit(String binding, 
								Document currentDcument,
								Customer customer,
								Map<String, Object> bean,
								Map<String, ComparisonComposite> bindingToNodes)
	throws Exception {
		ComparisonComposite node = bindingToNodes.get(binding);
		if (node == null) { // deleted entry
//			bindingToNodes.put(binding, createNode(owningReference, currentDocument, bean, false));
		}
		else { // existing entry
			updateNode(bean, node);
		}
	}
*/	
	private static ComparisonComposite createNode(Reference owningReference,
													Document currentDocument,
													Bean bean,
													boolean newNode)
	throws Exception {
		ComparisonComposite result = new ComparisonComposite();
		result.setBizId(bean.getBizId());
		result.setBusinessKeyDescription((bean instanceof PersistentBean) ? 
											((PersistentBean) bean).getBizKey() : 
											currentDocument.getSingularAlias());
		if (owningReference == null) {
			result.setReferenceName(null);
			result.setRelationshipDescription(currentDocument.getSingularAlias());
		}
		else {
			result.setReferenceName(owningReference.getName());
			result.setRelationshipDescription(owningReference.getDisplayName());
		}
		result.setMutation(newNode ? Mutation.added : Mutation.deleted);
		result.setDocument(currentDocument);
		addProperties(result, currentDocument, bean, newNode);

		return result;
	}

	private static void addProperties(ComparisonComposite node,
								Document beanDocument,
								Bean bean,
								boolean newEntry)
	throws Exception {
		for (Attribute attribute : beanDocument.getAttributes()) {
			if (! (attribute instanceof Reference)) {
				ComparisonProperty property = new ComparisonProperty();
				String name = attribute.getName();
				property.setName(name);
				property.setTitle(attribute.getDisplayName());
				property.setWidget(attribute.getDefaultInputWidget());

				Object value = BindUtil.get(bean, name);
				property.setNewValue(newEntry ? value : null);
				property.setOldValue(newEntry ? null : value);

				node.getProperties().add(property);
			}
		}
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
