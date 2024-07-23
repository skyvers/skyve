package modules.admin.Audit.models;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.comparison.ComparisonProperty;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditComparisonModel extends ComparisonModel<Audit, Audit> {
	@Override
	public ComparisonComposite getComparisonComposite(Audit me) throws Exception {
		Audit sourceVersion = me.getSourceVersion();
		Audit comparisonVersion = me.getComparisonVersion();
		
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		Customer c = u.getCustomer();
		Module am = null;
		Document ad = null;
		try {
			am = c.getModule(sourceVersion.getAuditModuleName());
			ad = am.getDocument(c, sourceVersion.getAuditDocumentName());
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// either the module or document is now inaccessible or no longer exists
		}
		
		boolean deleted = Operation.delete.equals(sourceVersion.getOperation());

		final Map<String, ComparisonComposite> bindingToNodes = new LinkedHashMap<>();
		
		// Visit the source audit record
		@SuppressWarnings("unchecked")
		Map<String, Object> source = (Map<String, Object>) JSON.unmarshall(u, sourceVersion.getAuditDetail());
		for (String binding : source.keySet()) {
			@SuppressWarnings("unchecked")
			Map<String, Object> sourceValues = (Map<String, Object>) source.get(binding);

			if (binding.isEmpty()) {
				bindingToNodes.put(binding, createNode(c, null, ad, sourceValues, deleted));
			}
			else {
				Reference reference = null;
				Document referenceDocument = null;
				try {
					if ((am != null) && (ad != null)) {
						TargetMetaData target = Binder.getMetaDataForBinding(c, am, ad, binding);
						reference = (Reference) target.getAttribute();
						Module targetModule = c.getModule(target.getDocument().getOwningModuleName());
						referenceDocument = (reference == null) ? null : targetModule.getDocument(c, reference.getDocumentName());
					}
				}
				catch (@SuppressWarnings("unused") MetaDataException e) {
					// couldn't resolve the binding; we'll continue on but it'll just be a node with the attribute names as audited
				}
				bindingToNodes.put(binding, createNode(c, reference, referenceDocument, sourceValues, deleted));
			}
		}
		
		// Visit the comparison audit record, if there is one
		if (comparisonVersion != null) {
			@SuppressWarnings("unchecked")
			Map<String, Object> compare = (Map<String, Object>) JSON.unmarshall(u, comparisonVersion.getAuditDetail());
			for (String binding : compare.keySet()) {
				ComparisonComposite node = bindingToNodes.get(binding);
				@SuppressWarnings("unchecked")
				Map<String, Object> compareValues = (Map<String, Object>) compare.get(binding);

				if (binding.isEmpty()) {
					if (node == null) {
						bindingToNodes.put(binding, createNode(c, null, ad, compareValues, true));
					}
					else {
						updateNode(node, c, compareValues);
					}
				}
				else {
					if (node == null) {
						TargetMetaData target = null;
						try {
							target = Binder.getMetaDataForBinding(c, am, ad, binding);
							Reference reference = (Reference) target.getAttribute();
							if (reference == null) {
								throw new MetaDataException("Can't create a new Audit node as binding " + binding + 
																" does not point to a reference.");
							}
							Module targetModule = c.getModule(target.getDocument().getOwningModuleName());
							Document referenceDocument = (am == null) ? null : targetModule.getDocument(c, reference.getDocumentName());
							bindingToNodes.put(binding, createNode(c, reference, referenceDocument, compareValues, true));
						}
						catch (@SuppressWarnings("unused") MetaDataException e) {
							bindingToNodes.put(binding, createNode(c, null, null, compareValues, true));
						}
					}
					else {
						updateNode(node, c, compareValues);
					}
				}
			}
		}

		// Link it all together
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

	private static ComparisonComposite createNode(Customer c,
													Reference owningReference,
													Document referenceDocument,
													Map<String, Object> values,
													boolean deleted)
	throws Exception {
		ComparisonComposite result = new ComparisonComposite();
		result.setBizId((String) values.remove(Bean.DOCUMENT_ID));
		String description = (String) values.remove(Bean.BIZ_KEY);
		if (description == null) {
			description = (referenceDocument == null) ? "" : referenceDocument.getLocalisedSingularAlias();
		}
		result.setBusinessKeyDescription(description);

		if (owningReference == null) {
			result.setReferenceName(null);
			result.setRelationshipDescription((referenceDocument == null) ? "" : referenceDocument.getLocalisedSingularAlias());
		}
		else {
			result.setReferenceName(owningReference.getName());
			result.setRelationshipDescription(owningReference.getLocalisedDisplayName());
		}
		result.setMutation(deleted ? Mutation.deleted:  Mutation.added);
		result.setDocument(referenceDocument);
		addProperties(c, result, values, deleted);

		return result;
	}

	private static void addProperties(Customer c,
										ComparisonComposite node,
										Map<String, Object> values,
										boolean deleted)
	throws Exception {
		Document nodeDocument = node.getDocument();
		List<ComparisonProperty> properties = node.getProperties();
		
		for (String name : values.keySet()) {
			Object value = values.get(name);

			ComparisonProperty property = new ComparisonProperty();
			property.setName(name);

			// Coerce the value to the attributes type if the attribute still exists
			Attribute attribute = null;
			if (nodeDocument != null) {
				// NB Attribute could be on a base document - use Binder
				Module nodeModule = c.getModule(nodeDocument.getOwningModuleName());
				try {
					TargetMetaData tmd = Binder.getMetaDataForBinding(c, nodeModule, nodeDocument, name);
					attribute = (tmd == null) ? null : tmd.getAttribute();
				}
				catch (@SuppressWarnings("unused") MetaDataException e) {
					// nothing to do here - The document no longer has the given attribute
				}
			}

			if (attribute == null) { // attribute DNE
				property.setTitle(name);
				property.setWidget(new TextField());
			}
			else { // attribute exists
				property.setTitle(attribute.getLocalisedDisplayName());
				property.setWidget(attribute.getDefaultInputWidget());

				Class<?> type = null;
				Converter<?> converter = null;
				if (attribute instanceof Enumeration) {
					Enumeration e = (Enumeration) attribute;
					e = e.getTarget();
					if (e.isDynamic()) {
						type = String.class;
						converter = new DynamicEnumerationConverter(e);
					}
					else {
						type = e.getEnum();
					}
				}
				else {
					type = attribute.getAttributeType().getImplementingType();
				}

				if (value instanceof String) {
					value = BindUtil.fromSerialised(converter, type, (String) value);
				}
				else {
					value = BindUtil.convert(type, value);
				}
			}

			property.setNewValue(deleted ? null : value);
			property.setOldValue(deleted ? value : null);

			properties.add(property);
		}
	}

	private static void updateNode(ComparisonComposite node,
									Customer c,
									Map<String, Object> values)
	throws Exception {
		if (values != null) {
			boolean nodeDirty = false;
			
			List<ComparisonProperty> properties = node.getProperties();
			for (ComparisonProperty property : properties) {
				String propertyName = property.getName();
				Object value = values.remove(propertyName);
				Document nodeDocument = node.getDocument();
				Attribute attribute = null;
				if (nodeDocument != null) {
					// NB Attribute could be on a base document - use Binder
					Module nodeModule = c.getModule(nodeDocument.getOwningModuleName());
					try {
						TargetMetaData tmd = Binder.getMetaDataForBinding(c, nodeModule, nodeDocument, propertyName);
						attribute = (tmd == null) ? null : tmd.getAttribute();
					}
					catch (@SuppressWarnings("unused") MetaDataException e) {
						// nothing to do here - The document no longer has the given attribute
					}
				}

				if (attribute != null) {
					Converter<?> converter = null;
					Class<?> type = null;
					if (attribute instanceof Enumeration) {
						Enumeration e = (Enumeration) attribute;
						e = e.getTarget();
						if (e.isDynamic()) {
							type = String.class;
							converter = new DynamicEnumerationConverter(e);
						}
						else {
							type = e.getEnum();
						}
					}
					else {
						type = attribute.getAttributeType().getImplementingType();
					}

					if (value instanceof String) {
						value = BindUtil.fromSerialised(converter, type, (String) value);
					}
					else {
						value = BindUtil.convert(type, value);
					}
				}

				property.setOldValue(value);
				if ((! nodeDirty) && property.isDirty()) {
					nodeDirty = true;
				}
			}

			values.remove(Bean.DOCUMENT_ID);
			values.remove(Bean.BIZ_KEY);
			
			// Process any extra old properties not present in the new version
			for (String name : values.keySet()) {
				ComparisonProperty property = new ComparisonProperty();
				property.setName(name);
				property.setTitle(name);
				property.setOldValue(values.get(name));
				properties.add(property);
				nodeDirty = true;
			}
			node.setMutation(nodeDirty ? Mutation.updated : Mutation.unchanged);
		}
	}
}
