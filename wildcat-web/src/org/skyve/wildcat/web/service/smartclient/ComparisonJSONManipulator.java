package org.skyve.wildcat.web.service.smartclient;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonProperty;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.generate.SmartClientGenerateUtils;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientFieldDefinition;
import org.skyve.wildcat.generate.SmartClientGenerateUtils.SmartClientLookupDefinition;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.model.document.CollectionImpl;
import org.skyve.wildcat.metadata.user.UserImpl;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;

/**
 * Creates something like :-
 * 
 *          [
 *               {bizId:"2", parent:"1", bizKey:"<span style='color:red'>Annual Return *</span>", icon:'icons/16/db_update.png', relationship:'Annual Return', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"3", parent:"2", bizKey:"Rob Brown", icon:'icons/16/db.png', relationship:'Owner', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"4", parent:"2", bizKey:"<span style='color:red'>Mike Sands *</span>", icon:'icons/16/db.png', relationship:'Operator', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"5", parent:"2", bizKey:"Rob Brown", icon:'icons/16/db.png', relationship:'Accounts', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"6", parent:"2", bizKey:"<span style='color:red'>Vineyards *</span>", icon:'icons/16/db_update.png', relationship:'Vineyards', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"7", parent:"6", bizKey:"Vineyard 2305 (West Block)", icon:'icons/16/db.png', relationship:'Vineyard', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"8", parent:"6", bizKey:"Vineyard 2307 (Old Red)", icon:'icons/16/db.png', relationship:'Vineyard', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"9", parent:"6", bizKey:"<span style='color:red'>Vineyard 2308 (Dry River) *</span>", icon:'icons/16/db_add.png', relationship:'Vineyard', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"10", parent:"2", bizKey:"<span style='color:red'>Parcels *</span>", icon:'icons/16/db_update.png', relationship:'Parcels', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"11", parent:"10", bizKey:"Parcel H701300 S599", icon:'icons/16/db.png', relationship:'Parcel', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"12", parent:"10", bizKey:"<span style='color:red'>Parcel H701300 S371 *</span>", icon:'icons/16/db_remove.png', relationship:'Parcel', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"13", parent:"10", bizKey:"Parcel H701300 S1099", icon:'icons/16/db.png', relationship:'Parcel', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"14", parent:"2", bizKey:"Plantings", icon:'icons/16/db.png', relationship:'Plantings', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"15", parent:"14", bizKey:"Cabernet Sauvignon", icon:'icons/16/db.png', relationship:'Planting', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"16", parent:"14", bizKey:"Parcel H701300 S371", icon:'icons/16/db.png', relationship:'Planting', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]},
 *               {bizId:"17", parent:"14", bizKey:"Parcel H701300 S1099", icon:'icons/16/db.png', relationship:'Planting', properties:[{title:'Vineyard Name', oldValue:'Poo', newValue:'Wee'}]}
 *           ]
 */
public final class ComparisonJSONManipulator {
	private UserImpl user;
	private CustomerImpl customer;
	private ComparisonComposite root;

	private static final String PARENT_KEY = "parent";
	private static final String BINDING_KEY = "_b";
	private static final String REFERENCE_TYPE_KEY = "_t";
	private static final String ICON_KEY = "icon";
	private static final String RELATIONSHIP_KEY = "relationship";
	private static final String PROPERTIES_KEY = "properties";
	private static final String NAME_KEY = "name";
	private static final String TITLE_KEY = "title";
	private static final String TYPE_KEY = "type";
	private static final String EDITOR_TYPE_KEY = "editorType";
	private static final String LENGTH_KEY = "length";
	private static final String VALUE_MAP_KEY = "valueMap";
	private static final String REQUIRED_KEY = "required";
	private static final String ALLOW_EMPTY_VALUE_KEY = "allowEmptyValue";
	private static final String OLD_VALUE_KEY = "oldValue";
	private static final String NEW_VALUE_KEY = "newValue";

	private static final String UNCHANGED_ICON = "icons/comparisonUnchanged.png";
	private static final String ADDED_ICON = "icons/comparisonAdded.png";
	private static final String DELETED_ICON = "icons/comparisonDeleted.png";
	private static final String UPDATED_ICON = "icons/comparisonUpdated.png";

	public ComparisonJSONManipulator(UserImpl user,
													CustomerImpl customer,
													ComparisonComposite root) {
		this.user = user;
		this.customer = customer;
		this.root = root;
	}

	public Iterable<Map<String, Object>> toJSONStructure() throws Exception {
		List<Map<String, Object>> result = new ArrayList<>();
		root.determineMutations();
		processNode(root, "", null, result);
		return result;
	}

	@SuppressWarnings("incomplete-switch")
	private void processNode(ComparisonComposite node,
								String referenceType,
								ComparisonComposite parent,
								List<Map<String, Object>> json)
	throws Exception {
		Document nodeDocument = node.getDocument();
		
		Map<String, Object> entry = new TreeMap<>();

		entry.put(Bean.DOCUMENT_ID, node.getBizId());
		entry.put(BINDING_KEY, node.getReferenceName());
		entry.put(REFERENCE_TYPE_KEY, referenceType);
		entry.put(PARENT_KEY, (parent == null) ? null : parent.getBizId());
		switch (node.getMutation()) {
		case added:
			entry.put(ICON_KEY, ADDED_ICON);
			entry.put(Bean.BIZ_KEY, showDirty(node.getBusinessKeyDescription()));
			break;
		case deleted:
			entry.put(ICON_KEY, DELETED_ICON);
			entry.put(Bean.BIZ_KEY, showDirty(node.getBusinessKeyDescription()));
			break;
		case unchanged:
			entry.put(ICON_KEY, UNCHANGED_ICON);
			entry.put(Bean.BIZ_KEY, node.getBusinessKeyDescription());
			break;
		case updated:
			entry.put(ICON_KEY, UPDATED_ICON);
			entry.put(Bean.BIZ_KEY, showDirty(node.getBusinessKeyDescription()));
			break;
		}
		entry.put(RELATIONSHIP_KEY, node.getRelationshipDescription());
		entry.put(PROPERTIES_KEY, listOfMapOfProperties(node.getProperties(), nodeDocument));
		
		json.add(entry);
		
		for (ComparisonComposite child : node.getChildren()) {
			String childReferenceType = "A";
			if (nodeDocument != null) {
				Reference childReference = nodeDocument.getReferenceByName(child.getReferenceName());
				if (childReference instanceof CollectionImpl) {
					childReferenceType = "C";
				}
			}
			processNode(child, childReferenceType, node, json);
		}
	}

	// property name -> JSON property map
	private List<Map<String, Object>> listOfMapOfProperties(List<ComparisonProperty> properties,
																Document nodeDocument)
	throws Exception {
		List<Map<String, Object>> result = new ArrayList<>(properties.size());

		for (ComparisonProperty property : properties) {
			Map<String, Object> item = new TreeMap<>();
			
			String propertyName = property.getName();
			item.put(NAME_KEY, propertyName);
			item.put(TITLE_KEY, property.isDirty() ? showDirty(property.getTitle()) : property.getTitle());
			
			if (nodeDocument != null) {
				// NB Need to use getMetaDataFroBinding() to ensure we find any attributes in base documents inherited
				Module module = customer.getModule(nodeDocument.getOwningModuleName());
				try {
					TargetMetaData target = Binder.getMetaDataForBinding(customer, module, nodeDocument, propertyName);
					Attribute attribute = target.getAttribute();
					InputWidget propertyWidget = property.getWidget();
					if (propertyWidget == null) {
						if (attribute == null) {
							throw new MetaDataException("No widget or attribute is defined");
						}
						
						propertyWidget = attribute.getDefaultInputWidget();
					}
					if ((propertyWidget != null) && (propertyWidget.getBinding() == null)) {
						propertyWidget.setBinding(propertyName);
					}
					
					SmartClientFieldDefinition field = SmartClientGenerateUtils.getField(user, customer, module, nodeDocument, propertyWidget);
					String type = field.getType();
					item.put(TYPE_KEY, type);
					String editorType = field.getEditorType();
					if (editorType != null) {
						item.put(EDITOR_TYPE_KEY, editorType);
					}
					Integer length = field.getLength();
		            if (length != null) {
						item.put(LENGTH_KEY, length);
		            }
		            if ((attribute != null) && DomainType.constant.equals(attribute.getDomainType())) {
			            Map<String, String> valueMap = SmartClientGenerateUtils.getConstantDomainValueMap(customer, nodeDocument, attribute);
			            if (valueMap != null) {
							item.put(VALUE_MAP_KEY, valueMap);
			            }
		            }
		            if (field.isRequired()) {
		            	item.put(REQUIRED_KEY, Boolean.TRUE);
		            }
		            else {
		                if ("select".equals(type) || "enum".equals(type)) {
			            	item.put(ALLOW_EMPTY_VALUE_KEY, Boolean.TRUE);
		                }
		            }
		
		            SmartClientLookupDefinition lookup = field.getLookup();
		            if (lookup != null) {
		        		PersistentBean oldValue = (PersistentBean) property.getOldValue();
		        		PersistentBean newValue = (PersistentBean) property.getNewValue();
		        		
		        		Map<String, String> valueMap = new TreeMap<>();
		        		if (oldValue != null) {
		        			String bizId = oldValue.getBizId();
		        			valueMap.put(bizId, oldValue.getBizKey());
		        			item.put(OLD_VALUE_KEY, bizId);
		        		}
		        		if (newValue != null) {
		        			String bizId = newValue.getBizId();
		        			valueMap.put(bizId, newValue.getBizKey());
		    				item.put(NEW_VALUE_KEY, bizId);
		        		}
						item.put(VALUE_MAP_KEY, valueMap);
						
		            }
		            else {
		    			item.put(NEW_VALUE_KEY, property.getNewValue());
		    			item.put(OLD_VALUE_KEY, property.getOldValue());
		            }
				}
				catch (MetaDataException e) { // not a real property, so bake something
	    			item.put(NEW_VALUE_KEY, property.getNewValue());
	    			item.put(OLD_VALUE_KEY, property.getOldValue());
					item.put(TYPE_KEY, "text");
				}
			}
			else {
    			item.put(NEW_VALUE_KEY, property.getNewValue());
    			item.put(OLD_VALUE_KEY, property.getOldValue());
				item.put(TYPE_KEY, "text");
			}
			
			result.add(item);
		}

		return result;
	}

	private static String showDirty(String label) {
		StringBuilder result = new StringBuilder(label);
		result.insert(0, "<span style='color:red'>");
		result.append(" *</span>");
		return result.toString();
	}
}
