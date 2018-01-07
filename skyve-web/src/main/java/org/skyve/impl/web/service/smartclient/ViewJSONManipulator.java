package org.skyve.impl.web.service.smartclient;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.reference.ContentReference;
import org.skyve.impl.metadata.view.reference.DefaultListViewReference;
import org.skyve.impl.metadata.view.reference.EditViewReference;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.ImplicitActionReference;
import org.skyve.impl.metadata.view.reference.QueryListViewReference;
import org.skyve.impl.metadata.view.reference.ReferenceProcessor;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReferenceTarget.ReferenceTargetType;
import org.skyve.impl.metadata.view.reference.ReportReference;
import org.skyve.impl.metadata.view.reference.ResourceReference;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.GeoLocator;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.DynamicImageServlet;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.comparison.ComparisonProperty;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.JSON;

// Note: We cannot cache the bindings required for each view as it may be different 
// depending on the security principal
class ViewJSONManipulator extends ViewVisitor {
	// Generate href expressions for references for smart client
	private class HrefProcessor extends ReferenceProcessor {
		@Override
		@SuppressWarnings("synthetic-access")
		public void processActionReference(ActionReference reference) {
			htmlGuts.append("javascript:").append(generateWidgetId());
			htmlGuts.append("._view.doAction('").append(reference.getActionName()).append("',false,'");
			htmlGuts.append(currentBindings.getBindingPrefix());
			htmlGuts.append("','{bizModule}','{bizDocument}','{bizId}')");
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processContentReference(ContentReference reference) {
			String binding = reference.getBinding();
			
			htmlGuts.append("content?").append(AbstractWebContext.REPORT_NAME).append("={").append(binding);
			htmlGuts.append("}&").append(AbstractWebContext.DOCUMENT_NAME).append("={bizModule}.{bizDocument}&");
			htmlGuts.append(AbstractWebContext.BINDING_NAME).append('=').append(binding).append("&_ctim=");
			htmlGuts.append(System.currentTimeMillis());
		}

		@Override
		public void processDefaultListViewReference(DefaultListViewReference reference) {
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processEditViewReference(EditViewReference reference) {
			String binding = reference.getBinding();
			htmlGuts.append(org.skyve.util.Util.getDocumentUrl(reference.getModuleName(),
																reference.getDocumentName(),
																(binding == null) ? 
																	null : 
																	new StringBuilder(64).append('{').append(binding).append('}').toString()));
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processExternalReference(ExternalReference reference) {
			htmlGuts.append(reference.getHref());
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processImplicitActionReference(ImplicitActionReference reference) {
			ImplicitActionName implicitAction = reference.getImplicitActionName();

			if (visitingDataWidget) {
				if (ImplicitActionName.Remove.equals(implicitAction)) {
					htmlGuts.append("javascript:").append(generateWidgetId());
					htmlGuts.append(".remove('{bizId}')");
				}
			}
		}

		@Override
		public void processQueryListViewReference(QueryListViewReference reference) {
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processReportReference(ReportReference reference) {
			htmlGuts.append("report.rpt?_format=").append(reference.getFormat()).append("&_id={bizId}");
			htmlGuts.append("&_n=").append(reference.getReportName());
			htmlGuts.append("&_doc=").append(reference.getModuleName()).append('.').append(reference.getDocumentName());
			for (Parameter parameter : reference.getParameters()) {
				htmlGuts.append('&').append(parameter.getName()).append('=');
				String stuff = parameter.getBinding();
				if (stuff != null) {
					htmlGuts.append('{').append(stuff).append('}');
				}
				else {
					stuff = parameter.getValue();
					if (stuff != null) {
						htmlGuts.append(stuff);
					}
				}
			}
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public void processResourceReference(ResourceReference reference) {
			htmlGuts.append("resources?").append(AbstractWebContext.DOCUMENT_NAME).append("={bizModule}.{bizDocument}&");
			htmlGuts.append(AbstractWebContext.REPORT_NAME).append('=').append(reference.getRelativeFile());
		}
		
		@SuppressWarnings("synthetic-access")
		private String generateWidgetId() {
			StringBuilder id = new StringBuilder(64);

			String name = view.getName();
			id.append(module.getName()).append('_');
			id.append(document.getName()).append('_');
			id.append(name).append('_');
			if (ViewType.create.toString().equals(name)) {
				id.append(createIdCounter);
			}
			else {
				id.append(editIdCounter);
			}
			
			return id.toString();
		}
	}
	
	private User user;
	private Bean bean;
	// we are applying values, so don't include bindings for
	// disabled widgets or lookup description bindings
	private boolean forApply;
	
	// The bindings required to be resolved to display this view 
	// for the document in its current state for the current user
	// This a composite tree where the root node represents top level bindings,
	// The sub-nodes are either associations or collections that must be processed recursively.
	private ViewBindings bindingTree = new ViewBindings(document);

	// The current binding getting processed at present
	// NB This is initially set to the root node until we traverse down through a data grid or comparison.
	private ViewBindings currentBindings = bindingTree;
	
	// This is the value maps required for variant and dynamic domain values
	private Map<String, LinkedHashMap<String, String>> valueMaps = new TreeMap<>();
	
	// These are format strings keyed by the binding prefix
	// binding prefix ("" if no prefix - see bindings above), and then a map of 
	// form/grid column item names -> format expressions 
	private Map<String, Map<String, String>> formats = new TreeMap<>();

	// There is no binding prefixes for comparisons - too hard
	// The map is for comparison bindings -> comparison JSON tree required to populate the widget
	private Map<String, Iterable<Map<String, Object>>> comparisons = new TreeMap<>();
	
	// This is used to assign names to boilerplate values which have binding expressions in them, such as links.
	// These values are evaluated server side and stashed in the JSON response under a bogus binding.
	private int formatCounter = 0;
	
	// This is used to generate smart client widget IDs that correspond to what is generated,
	// so that code generated server-side can look up the appropriate controls
	private int editIdCounter = 0;
	private int createIdCounter = 0;

	// HrefProcessor - takes a Reference and appends a suitable href in htmlGuts
	@SuppressWarnings("synthetic-access")
	HrefProcessor hrefProcessor = new HrefProcessor();
	private StringBuilder htmlGuts = new StringBuilder(64);

	ViewJSONManipulator(User user,
							Module module, 
							Document document, 
							View view,
							Bean bean,
							int editIdCounter, // the base number which is incremented for view component IDs for uniqueness
							int createIdCounter, // the base number which is incremented for view component IDs for uniqueness
							boolean forApply) {
		super((CustomerImpl) user.getCustomer(),
				(ModuleImpl) module,
				(DocumentImpl) document,
				(ViewImpl) view);
		this.user = user;
		this.bean = bean;
		this.editIdCounter = editIdCounter;
		this.createIdCounter = createIdCounter;
		this.forApply = forApply;
	}
	
	String toJSON(AbstractWebContext webContextToReference)
	throws Exception {
		Map<String, Object> result = new TreeMap<>();

		// Encode the context bean
		String webId = webContextToReference.getWebId();
		result.put(AbstractWebContext.CONTEXT_NAME, webId);
		
		// put the view title in
		result.put("_title", 
					BindUtil.formatMessage(user.getCustomer(), 
											view.getTitle(), 
											bean));

		// put the view changed/dirty flag in
		result.put("_changed", Boolean.valueOf(UtilImpl.hasChanged(webContextToReference.getCurrentBean())));

		// put web context growls and messages in
		List<Map<String, String>> messages = webContextToReference.getGrowls();
		if (messages != null) {
			result.put("_growls", messages);
		}
		messages = webContextToReference.getMessages();
		if (messages != null) {
			result.put("_messages", messages);
		}
		
		constructJSONObjectFromBinding(bindingTree, result, webId);
		
		if (! valueMaps.isEmpty()) {
			result.put("_valueMaps", valueMaps);
		}
		
		for (String binding : comparisons.keySet()) {
			result.put(binding, comparisons.get(binding));
		}
		
		return JSON.marshall(user.getCustomer(), result, null);
	}
	
	private void constructJSONObjectFromBinding(ViewBindings bindings,
													Map<String, Object> json,
													String webId)
	throws Exception {
		String bindingPrefix = bindings.getBindingPrefix();
		if (bindingPrefix == null) { // root node (top level)
			addBindingsAndFormatValues(bindings, bean, json, webId);
		}
		else { // reference bindings (sub-bindings)
			Object value = BindUtil.get(bean, bindingPrefix);
			if (value instanceof List) {
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) value;
				List<Map<String, Object>> values = new ArrayList<>(list.size());
				for (Bean element : list) {
					Map<String, Object> elementValues = new TreeMap<>();
					addBindingsAndFormatValues(bindings, element, elementValues, webId);
					values.add(elementValues);
				}
				json.put(BindUtil.sanitiseBinding(bindingPrefix), values);
			}
			else {
				Bean currentBean = (Bean) value;
				Map<String, Object> beanValues = new TreeMap<>();
				addBindingsAndFormatValues(bindings, currentBean, beanValues, webId);
				json.put(BindUtil.sanitiseBinding(bindingPrefix), beanValues);
			}
		}
	}
	
	private void addBindingsAndFormatValues(ViewBindings bindings,
												Bean aBean,
												Map<String, Object> toAddTo,
												String webId)
	throws Exception {
		// Add bindings
		for (String binding : bindings.getBindings()) {
			Object value = BindUtil.get(aBean, binding);
			if (value instanceof Bean) {
				value = ((Bean) value).getBizId();
			}
			// Coerce boolean and numbers into strings if they have a domain defined
			// because SmartClient needs strings in its FormItem "valueMap" property 
			// and the item value has to match for a domain value to be selected.
			else if ((value instanceof Boolean) || (value instanceof Number)) {
				Attribute attribute = null;
				try {
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
					attribute = (target != null) ? target.getAttribute() : null;
				}
				catch (MetaDataException e) {
					// not an attribute
				}
				if ((attribute != null) && (attribute.getDomainType() != null)) {
					value = value.toString();
				}
			}
			toAddTo.put(BindUtil.sanitiseBinding(binding), value);
		}
		
		// Add formats
		String bindingKey = bindings.getBindingPrefix();
		if (bindingKey == null) {
			bindingKey = "";
		}
		Map<String, String> formatMap = formats.get(bindingKey);
		if (formatMap != null) {
			for (String name : formatMap.keySet()) {
				String format = formatMap.get(name);
				// replace the "{CONTEXT}" placeholder with the current webId
				format = format.replace("{CONTEXT}", webId);
				// now format the message
				format = BindUtil.formatMessage(user.getCustomer(), format, aBean);
				// remove the display style if its true
				format = format.replace("display:true;", "");
				// change to display none if display style is false
				format = format.replace("display:false;", "display:none;");
				// change any null valued image urls to point to blank
				format = format.replaceAll("src=\\\"content\\?_n=&_doc=.*?\\\"", "src=\"images/blank.gif\"");
				toAddTo.put(name, format);
			}
		}
		
		for (String childBinding : bindings.getChildren()) {
			constructJSONObjectFromBinding(bindings.putOrGetChild(childBinding, null), toAddTo, webId);
		}
	}
/*	
	private static void displayViewBindings(ViewBindings bindings) {
		for (String binding : bindings.getBindings()) {
			Util.LOGGER.info(bindings.getFullyQualifiedBindingPrefix() + " - " + bindings.getBindingPrefix() + " : " + binding);
		}
		
		for (String binding : bindings.getChildren()) {
			ViewBindings childViewBindings = bindings.putOrGetChild(binding, null);
			displayViewBindings(childViewBindings);
		}
	}
*/	
	void applyJSON(String json, AbstractPersistence persistence) throws Exception {
		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) JSON.unmarshall(user, json);

//		displayViewBindings(bindingTree);
		applyJSON(bindingTree, document, values, bean, persistence);
	}

	@SuppressWarnings("unchecked")
	private void applyJSON(ViewBindings bindings,
							Document appliedToDoc,
							Map<String, Object> values,
							Bean appliedTo,
							AbstractPersistence persistence)
	throws Exception {
//Util.LOGGER.info("FQ BINDING PREFIX = " + bindings.getFullyQualifiedBindingPrefix());

		applyJSONProperties(bindings, appliedToDoc, values, appliedTo, persistence);

		for (String childBindingPrefix : bindings.getChildren()) {
			ViewBindings childBindings = bindings.putOrGetChild(childBindingPrefix, null);
			// Get the reference target metadata
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																	module, 
																	appliedToDoc, 
																	childBindingPrefix);
			Relation relation = (Relation) target.getAttribute();
			Document relatedDocument = module.getDocument(customer, relation.getDocumentName());
			AttributeType relationType = relation.getAttributeType();
			if (List.class.equals(relationType.getImplementingType())) { // relation is a collection (or many to many inverse)
				// We get the JSON list and apply all elements to the existing elements
				// in the persisted list.
				//*Any persisted elements that are not present in the JSON list are removed
				//*Any JSON elements that are not present in the persisted elements are 
				// added to the persisted list
				
				// The list of values from the JSON post
				// If the rows are references (membership widgets),
				// the rows should exist this will be a List of bizId Strings.
				// If the rows are aggregated/composed (grid widget),
				// then each row property is updated to add or modify the rows.
				List<Object> requestList = (List<Object>) BindUtil.get(values, BindUtil.unsanitiseBinding(childBindingPrefix));
				// If the requestList is null then it was not sent from the client - it is irrelevant.
				// A data grid binding can be struck out of the request when a zoom out occurs.
				// This ensures that old values are not updated when the view is refreshed - see EditView.js where action == 'ZoomOut'
				if (requestList == null) {
					continue;
				}

				// sort requestList by bizOrdinal if appropriate
//				if (! requestList.isEmpty()) {
//					Object first = requestList.get(0);
//					if (first instanceof Map) {
//						if (((Map<String, Object>) first).containsKey(ChildBean.ORDINAL_KEY)) {
//							BindUtil.sortCollectionByOrdering(requestList, CORE.newOrdering(ChildBean.ORDINAL_KEY, SortDirection.ascending));
//						}
//					}
//				}
				
				// Get the existing beans in the list
				List<Bean> beanList = (List<Bean>) BindUtil.get(appliedTo, childBindingPrefix);
				// beanList could be null if we have a datagrid that is bound to a compound binding...
				// eg <dataGrid binding="foo.bars" />
				// Now, if foo is made null by another control - maybe a combo empty value is chosen then
				// 1) foo is null
				// 2) foo.bars yields null and we don't need to apply any processing
				if (beanList != null) {
					// process the existing beans applying the changes and ordering from the requestList
					int newIndex = 0;
					for (Object requestListItem : requestList) {
						String thisBizId = null;
						Map<String, Object> thisMap = null;
						if (requestListItem instanceof String) { // reference
							thisBizId = (String) requestListItem;
						}
						else {
							thisMap = (Map<String, Object>) requestListItem;
							thisBizId = (String) thisMap.get(Bean.DOCUMENT_ID);
						}
						Bean thisBean = null;
						if (thisBizId != null) {
							thisBean = BindUtil.getElementInCollection(beanList, thisBizId);
						}
	
						if (thisBean == null) { // DNE in beanList
							if (thisMap == null) { // reference
								thisBean = WebUtil.findReferencedBean(relatedDocument, thisBizId, persistence);
							}
							else {
								// create a new one with new Instance
								thisBean = relatedDocument.newInstance(user);
								applyJSON(childBindings,
											relatedDocument,
											thisMap,
											thisBean,
											persistence);
							}
	
							// Determine whether link the bean in as the parent
							String parentDocumentName = relatedDocument.getParentDocumentName();
							if ((parentDocumentName != null) && // is a child document
									parentDocumentName.equals(appliedToDoc.getName())) { // and bean is a compatible parent
								((ChildBean<Bean>) thisBean).setParent(appliedTo);
							}
							// Determine whether to set the other side of an inverse
							if (AttributeType.inverseMany.equals(relationType)) {
								Inverse inverse = (Inverse) relation;
								if (Boolean.TRUE.equals(inverse.getCascade())) {
									String referenceName = inverse.getReferenceName();
									// Get the reference target metadata - NB could be inherited
									target = BindUtil.getMetaDataForBinding(customer, module, relatedDocument, referenceName);
									Attribute reference = target.getAttribute();
									AttributeType referenceType = reference.getAttributeType();
									if (AttributeType.association.equals(referenceType)) { // association
										BindUtil.set(thisBean, referenceName, appliedTo);
									}
									else { // collection
										List<Bean> referenceList = (List<Bean>) BindUtil.get(thisBean, referenceName);
										if (! referenceList.contains(thisBean)) {
											referenceList.add(appliedTo);
										}
									}
								}
							}
							
							beanList.add(newIndex, thisBean);
						}
						else { // found
							// Only move the bean in the collection if required
							// NB We do this conditionally so we don't upset hibernate collection dirtiness
							if (beanList.indexOf(thisBean) != newIndex) {
								beanList.remove(thisBean);
								beanList.add(newIndex, thisBean);
							}
	
							// apply the properties from the JSON to the bean element, if its not a reference
							if (thisMap != null) {
								applyJSON(childBindings,
											relatedDocument,
											thisMap,
											thisBean,
											persistence);
							}
						}
						newIndex++;
					}
	
					// delete any left over beans in the list as these were not present in the requestList
					while (beanList.size() > newIndex) {
						Bean removed = beanList.remove(newIndex);

						// Determine whether to null the other side of an inverse
						if (AttributeType.inverseMany.equals(relationType)) {
							Inverse inverse = (Inverse) relation;
							if (Boolean.TRUE.equals(inverse.getCascade())) {
								String referenceName = inverse.getReferenceName();
								// Get the reference target metadata - NB could be inherited
								target = BindUtil.getMetaDataForBinding(customer, module, relatedDocument, referenceName);
								Attribute reference = target.getAttribute();
								AttributeType referenceType = reference.getAttributeType();
								if (AttributeType.association.equals(referenceType)) { // association
									BindUtil.set(removed, referenceName, null);
								}
								else { // collection
									List<Bean> referenceList = (List<Bean>) BindUtil.get(removed, referenceName);
									while (referenceList.contains(removed)) {
										referenceList.remove(appliedTo);
									}
								}
							}
						}
					}
					
					if (relation instanceof Collection) { // NB it could be an inverse
						BindUtil.sortCollectionByMetaData(appliedTo, customer, module, appliedToDoc, childBindingPrefix);
					}
				}
			}
			else { // relation is an association (or one to one / one to many inverse)
				// Get the existing bean referenced
				Bean referencedBean = (Bean) BindUtil.get(appliedTo, childBindingPrefix);
				Object requestObject = BindUtil.get(values, BindUtil.unsanitiseBinding(childBindingPrefix));
				if (requestObject == null) {
					if (referencedBean != null) {
						BindUtil.set(appliedTo, childBindingPrefix, null);
						
						// Determine whether to null the other side of an inverse
						if (AttributeType.inverseOne.equals(relationType)) {
							Inverse inverse = (Inverse) relation;
							if (Boolean.TRUE.equals(inverse.getCascade())) {
								String referenceName = inverse.getReferenceName();
								// Get the reference target metadata - NB could be inherited
								target = BindUtil.getMetaDataForBinding(customer, module, relatedDocument, referenceName);
								Attribute reference = target.getAttribute();
								AttributeType referenceType = reference.getAttributeType();
								if (AttributeType.association.equals(referenceType)) { // association
									BindUtil.set(referencedBean, referenceName, null);
								}
								else { // collection
									List<Bean> referenceList = (List<Bean>) BindUtil.get(referencedBean, referenceName);
									while (referenceList.contains(appliedTo)) {
										referenceList.remove(appliedTo);
									}
								}
							}
						}
					}
				}
				else {
					if (requestObject instanceof String) { // a bizId
						String requestBizId = (String) requestObject;
						// find the existing bean with retrieve if not the same as in the request
						if ((referencedBean == null) || (! referencedBean.getBizId().equals(requestBizId))) {
							Bean oldReferencedBean = referencedBean;
							referencedBean = WebUtil.findReferencedBean(relatedDocument, requestBizId, persistence);
							BindUtil.set(appliedTo, childBindingPrefix, referencedBean);

							// Determine whether to set the other side of an inverse
							if (AttributeType.inverseOne.equals(relationType)) {
								Inverse inverse = (Inverse) relation;
								if (Boolean.TRUE.equals(inverse.getCascade())) {
									String referenceName = inverse.getReferenceName();
									// Get the reference target metadata - NB could be inherited
									target = BindUtil.getMetaDataForBinding(customer, module, relatedDocument, referenceName);
									Attribute reference = target.getAttribute();
									AttributeType referenceType = reference.getAttributeType();
									if (AttributeType.association.equals(referenceType)) { // association
										// Null out the other side of the inverse for the old inverse value
										if (oldReferencedBean != null) {
											BindUtil.set(oldReferencedBean, referenceName, null);
										}
										// Set the other side of the inverse for the new inverse value
										BindUtil.set(referencedBean, referenceName, appliedTo);
									}
									else { // collection
										// Remove elements that contain the other side of the inverse for the old inverse value
										if (oldReferencedBean != null) {
											List<Bean> referenceList = (List<Bean>) BindUtil.get(oldReferencedBean, referenceName);
											while (referenceList.contains(appliedTo)) {
												referenceList.remove(appliedTo);
											}
										}
										List<Bean> referenceList = (List<Bean>) BindUtil.get(referencedBean, referenceName);
										if (! referenceList.contains(appliedTo)) {
											referenceList.add(appliedTo);
										}
									}
								}
							}
						}
					}
					else { // a JSON object
						Map<String, Object> referencedMap = (Map<String, Object>) requestObject;
						
						// create a new one if required
						if (referencedBean == null) {
							referencedBean = relatedDocument.newInstance(user);
						}
						applyJSON(childBindings, relatedDocument, referencedMap, referencedBean, persistence);
					}
				}
			}
		}
	}

	private void applyJSONProperties(ViewBindings bindings,
										Document documentToApply,
										Map<String, Object> valuesToApply,
										Bean beanToApplyTo,
										AbstractPersistence persistence)
	throws Exception {
		for (String binding : bindings.getBindings()) {
//Util.LOGGER.info(currentBindings.getFullyQualifiedBindingPrefix() + " : " + binding);
			if (bindings.isWritable(binding)) {
				applyJSONProperty(documentToApply, binding, valuesToApply, beanToApplyTo, persistence);
			}
		}
	}
	
	private void applyJSONProperty(Document startingDocument,
									String binding, 
									Map<String, Object> values, 
									Bean targetBean,
									AbstractPersistence persistence) 
	throws Exception {
		String valueKey = BindUtil.sanitiseBinding(binding);
		if (! values.containsKey(valueKey)) {
			return;
		}
		
		try {
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																	module, 
																	startingDocument, 
																	binding);
			Attribute attribute = target.getAttribute();
			if ((attribute instanceof Association) || (attribute instanceof InverseOne)) {
				String documentName = ((Relation) attribute).getDocumentName();
				Document relatedDocument = module.getDocument(customer, documentName);

				Bean oldRelatedBean = (Bean) BindUtil.get(targetBean, binding);
				Bean newRelatedBean = null;
				boolean dirty = false;
				
				// put the new related bean (or null) into the values
				Object relatedValue = values.get(valueKey);
				if (relatedValue == null) {
					dirty = (oldRelatedBean != null);
				}
				// Don't try to traverse an embedded association or inverseOne object here recursively.
				// The correct bindings are created when visiting the view during the apply.
				// So here we only need to effect the replacement of bizId Strings with retrieved objects
				else if (relatedValue instanceof String) { // a bizId (not a JSON object)
					String relatedId = (String) relatedValue;
					// old value id and new value id are different
					if ((oldRelatedBean == null) || (! oldRelatedBean.getBizId().equals(relatedId))) {
						newRelatedBean = WebUtil.findReferencedBean(relatedDocument, relatedId, persistence);
						dirty = true;
					}
				}
				
				if (dirty) {
					BindUtil.populateProperty(user, targetBean, binding, newRelatedBean, true);

					// if this is a cascaded inverse, set the other side
					if (attribute instanceof Inverse) {
						Inverse inverse = (Inverse) attribute;
						if (Boolean.TRUE.equals(inverse.getCascade())) {
							// clear out the old relation
							if (oldRelatedBean != null) {
								BindUtil.set(oldRelatedBean, inverse.getReferenceName(), null);
							}
							// Set the new relation
							if (newRelatedBean != null) {
								BindUtil.set(newRelatedBean, inverse.getReferenceName(), targetBean);
							}
						}
						else {
							UtilImpl.LOGGER.warning(String.format("A new value <%s> was set on binding '%s' but [cascading] " + 
																		"is not enabled on the inverseOne attribute, so it will not be persisted.",
																	relatedValue,
																	binding));
						}
					}
				}
			}
			// We have a binding to a document attribute OR
			// we have a binding to an implicit property that is settable.
			else if ((attribute != null) ||
					binding.endsWith(PersistentBean.FLAG_COMMENT_NAME) ||
					binding.endsWith(Bean.ORDINAL_NAME)) {
//Util.LOGGER.info("SET " + targetBean + '.' + binding + " = " + values.get(valueKey));
				BindUtil.populateProperty(user, 
											targetBean, 
											binding, 
											values.get(valueKey), 
											true);
			}
		}
		catch (MetaDataException e) {
			// do nothing useful as the binding isn't an attribute
			UtilImpl.LOGGER.warning(e.toString());
		}
	}
	
	private void addBinding(String binding, boolean writeable) {
		addBinding(binding, writeable, false);
	}

	private void addBinding(String binding, boolean writable, boolean noPrefix) {
		if (binding != null) {
		    ViewBindings bindings = (noPrefix ? bindingTree : currentBindings);
			bindings.putBinding(binding, writable);
		}
	}

	private void addFormat(String valueTemplate) {
		if (valueTemplate != null) {
			String currentBindingPrefix = currentBindings.getBindingPrefix();
			String formatKey = (currentBindingPrefix == null) ? "" : currentBindingPrefix;
			Map<String, String> formatMap = formats.get(formatKey);
			if (formatMap == null) {
				formatMap = new TreeMap<>();
				formats.put(formatKey, formatMap);
			}
			String name = "_" + formatCounter++;
			formatMap.put(name, valueTemplate);
		}
	}

	private void addCondition(String condition) {
		if ((condition != null) && (! condition.equals("true")) && (! condition.equals("false"))) {
			addBinding(condition, false, true);
		}
	}
	
	/**
	 * Put variant and dynamic domain values in the valueMaps variable
	 * (if not there already) for inclusion in the instance.
	 * 
	 * @param binding	The binding for the domain values lookup.
	 */
	private void putVariantAndDynamicDomainValuesInValueMaps(String binding) {
		String safeBinding = BindUtil.sanitiseBinding(binding);
		if (! valueMaps.containsKey(safeBinding)) {
            TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
            Attribute attribute = target.getAttribute();
            DomainType domainType = attribute.getDomainType();

            // Keep the domain values ordered with a LinkedHashMap
            LinkedHashMap<String, String> values = new LinkedHashMap<>();

            if (DomainType.variant.equals(domainType)) {
            	DocumentImpl targetDocument = (DocumentImpl) target.getDocument();
                for (DomainValue domainValue : targetDocument.getDomainValues(customer, 
    																			domainType, 
    																			attribute, 
    																			null)) {
                    values.put(domainValue.getCode(), domainValue.getDescription());
                }
                valueMaps.put(safeBinding, values);
            }
            else if (DomainType.dynamic.equals(domainType)) {
            	List<DomainValue> domainValues = null;
            	
            	int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex >= 0) {
					Bean owningBean = null;
					try {
						// This could be a collection!!!
						// for example in a view with <datagrid binding="collectionName"><column binding="dynamic"/></dataGrid>
						// the binding is "collectionName.dynamic".
						Object owner = BindUtil.get(bean, binding.substring(0, lastDotIndex));
						if (owner instanceof Bean) {
							owningBean = (Bean) owner;
						}
					}
					catch (Exception e) {
						throw new MetaDataException("Could not get owning bean to create dynamic values", e);
					}
					if (owningBean != null) {
		            	DocumentImpl targetDocument = (DocumentImpl) target.getDocument();
						domainValues = targetDocument.getDomainValues(customer,
																		domainType,
																		attribute,
																		owningBean);
					}
				}
				else {
					domainValues = document.getDomainValues(customer, domainType, attribute, bean);						
				}
                
				if (domainValues != null) {
					for (DomainValue domainValue : domainValues) {
	                    values.put(domainValue.getCode(), domainValue.getDescription());
					}
	                valueMaps.put(safeBinding, values);
				}
            }
        }
	}
	
	@Override
	protected boolean visible(Invisible invisible) {
		return evaluateConditionInOppositeSense(invisible.getInvisibleConditionName());
	}

	@Override
	protected boolean enabled(Disableable disableable) {
		return evaluateConditionInOppositeSense(disableable.getDisabledConditionName());
	}
	
	private boolean evaluateConditionInOppositeSense(String conditionName) {
		boolean result = true;

		if (conditionName != null) {
			if ("true".equals(conditionName)) {
				result = false;
			}
			else if (! "false".equals(conditionName)) {
				result = ! bean.evaluateCondition(conditionName);
			}
		}
		
		return result;
	}

	@Override
	public void visitView() {
		addCondition(Bean.PERSISTED_KEY); // for inplicit actions in toolbar
		addCondition(Bean.CREATED_KEY); // for create/edit view to operate
		addCondition(Bean.NOT_CREATED_KEY); // for create/edit view to operate
	}

	@Override
	public void visitTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
		addCondition(tabPane.getInvisibleConditionName());
		addCondition(tabPane.getDisabledConditionName());
		addBinding(tabPane.getSelectedTabIndexBinding(), false);
	}

	@Override
	public void visitedTabPane(TabPane tabPane,
								boolean parentVisible,
								boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
		addCondition(tab.getInvisibleConditionName());
		addCondition(tab.getDisabledConditionName());
	}

	@Override
	public void visitedTab(Tab tab,
							boolean parentVisible,
							boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitVBox(VBox vbox,
							boolean parentVisible,
							boolean parentEnabled) {
		addCondition(vbox.getInvisibleConditionName());
	}

	@Override
	public void visitHBox(HBox hbox,
							boolean parentVisible,
							boolean parentEnabled) {
		addCondition(hbox.getInvisibleConditionName());
	}

	@Override
	public void visitForm(Form form,
							boolean parentVisible,
							boolean parentEnabled) {
		addCondition(form.getDisabledConditionName());
		addCondition(form.getInvisibleConditionName());
	}

	@Override
	public void visitedForm(Form form,
								boolean parentVisible,
								boolean parentEnabled) {
		// keep this in sync with the generated edit views
		if (ViewType.create.toString().equals(view.getName())) {
			createIdCounter++;
		}
		else {
			editIdCounter++;
		}
	}

	@Override
	public void visitFormColumn(FormColumn column,
									boolean parentVisible,
									boolean parentEnabled) {
		// not bound
	}

	@Override
	public void visitFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		// not bound
	}

	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// not bound
	}

	@Override
	public void visitedFormItem(FormItem item,
								boolean parentVisible,
								boolean parentEnabled) {
		// not bound
	}

	@Override
	public void visitedFormRow(FormRow row,
								boolean parentVisible,
								boolean parentEnabled) {
		// not bound
	}

	@Override
	public void visitButton(Button button,
								boolean parentVisible,
								boolean parentEnabled) {
		Action action = view.getAction(button.getActionName());
		addCondition(action.getInvisibleConditionName());
		addCondition(action.getDisabledConditionName());
	}
	
	@Override
	public void visitGeoLocator(GeoLocator locator,
									boolean parentVisible,
									boolean parentEnabled) {
		addCondition(locator.getInvisibleConditionName());
		addCondition(locator.getDisabledConditionName());
	}

	@Override
	public void visitGeometry(Geometry geometry,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}
		
		if (parentVisible && visible(geometry)) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addBinding(geometry.getBinding(), true);
			}
		}
		addCondition(geometry.getInvisibleConditionName());
		addCondition(geometry.getDisabledConditionName());
	}

	@Override
	public void visitMap(MapDisplay map,
							boolean parentVisible,
							boolean parentEnabled) {
		addCondition(map.getInvisibleConditionName());
	}

	@Override
	public void visitDialogButton(DialogButton button,
									boolean parentVisible,
									boolean parentEnabled) {
		addCondition(button.getInvisibleConditionName());
		addCondition(button.getDisabledConditionName());
	}

	@Override
	public void visitDynamicImage(DynamicImage image,
									boolean parentVisible,
									boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
			// TODO - should make the URL dependant on the image format
			htmlGuts.append("<img src=\"dynamic.png?_n=").append(image.getName());
			htmlGuts.append("&_doc={bizModule}.{bizDocument}");
			
			Integer pixelWidth = image.getPixelHeight();
			Integer pixelHeight = image.getPixelHeight();
			Integer initialPixelWidth = image.getImageInitialPixelWidth();
			Integer initialPixelHeight = image.getImageInitialPixelHeight();
			if (pixelWidth != null) {
				htmlGuts.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(pixelWidth);
			}
			else {
				htmlGuts.append('&').append(DynamicImageServlet.IMAGE_WIDTH_NAME).append('=').append(initialPixelWidth);
			}
			if (pixelHeight != null) {
				htmlGuts.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(pixelHeight);
			}
			else {
				htmlGuts.append('&').append(DynamicImageServlet.IMAGE_HEIGHT_NAME).append('=').append(initialPixelHeight);
			}
			htmlGuts.append('&').append(DynamicImageServlet.IMAGE_WIDTH_ZOOM_NAME).append("=100&");
			htmlGuts.append(DynamicImageServlet.IMAGE_HEIGHT_ZOOM_NAME).append("=100&");

			htmlGuts.append(AbstractWebContext.CONTEXT_NAME).append("={CONTEXT}&");
			htmlGuts.append(Bean.DOCUMENT_ID).append("={bizId}");
			appendHtmlGutsStyle(image.getPixelWidth(), image.getPixelHeight(), null, image.getInvisibleConditionName());
			htmlGuts.append("/>");
		}
		else {
			addCondition(image.getInvisibleConditionName());
		}
	}

	@Override
	public void visitSpacer(Spacer spacer) {
		// nothing to do here
	}

	@Override
	public void visitStaticImage(StaticImage image,
									boolean parentVisible,
									boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
			htmlGuts.append("<img src=\"resources?_n=").append(image.getRelativeFile());
			htmlGuts.append("&_doc={bizModule}.{bizDocument}&_b=null\"");
			appendHtmlGutsStyle(image.getPixelWidth(), image.getPixelHeight(), null, image.getInvisibleConditionName());
			htmlGuts.append("/>");
		}
		else {
			addCondition(image.getInvisibleConditionName());
		}
	}

	@Override
	public void visitContentImage(ContentImage image,
									boolean parentVisible, 
									boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
			String binding = image.getBinding();
			htmlGuts.append("<img src=\"content?_n={").append(binding);
			htmlGuts.append("}&_doc={bizModule}.{bizDocument}&_b=");
			htmlGuts.append(binding).append('"');
			appendHtmlGutsStyle(image.getPixelWidth(), image.getPixelHeight(), null, image.getInvisibleConditionName());
			htmlGuts.append("/>");
		}
		else {
			if (parentVisible && visible(image)) {
				if ((! forApply) || 
						(forApply && parentEnabled && (! Boolean.FALSE.equals(image.getEditable())))) {
					addBinding(image.getBinding(), true);
				}
			}
			addCondition(image.getInvisibleConditionName());
			addBinding(Bean.MODULE_KEY, false);
			addBinding(Bean.DOCUMENT_KEY, false);
			addBinding(Bean.DATA_GROUP_ID, false);
			addBinding(Bean.USER_ID, false);
		}
	}

	@Override
	public void visitBlurb(Blurb blurb,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
			htmlGuts.append("<div");
			appendHtmlGutsStyle(blurb.getPixelWidth(),
									blurb.getPixelHeight(),
									blurb.getTextAlignment(),
									blurb.getInvisibleConditionName());
			htmlGuts.append('>').append(blurb.getMarkup()).append("</div>");
		}
		else {
			String markup = blurb.getMarkup();
			if (BindUtil.messageIsBound(markup)) { // has a binding expression
				if (parentVisible && visible(blurb)) {
					if ((! forApply) || 
							(forApply && parentEnabled)) {
						addFormat(markup);
					}
					else {
						// ensure the format counter is incremented to stay in sync with the generated edit view
						formatCounter++;
					}
				}
				else {
					// ensure the format counter is incremented to stay in sync with the generated edit view
					formatCounter++;
				}
			}
			addCondition(blurb.getInvisibleConditionName());
		}
	}

	@Override
	public void visitLabel(Label label,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
			htmlGuts.append("<span");
			appendHtmlGutsStyle(label.getPixelWidth(),
									label.getPixelHeight(),
									label.getTextAlignment(),
									label.getInvisibleConditionName());
			htmlGuts.append('>');
			String binding = label.getBinding();
			if (binding != null) {
				htmlGuts.append('{').append(binding).append('}');
			}
			else {
				String value = label.getValue();
				if (value != null) {
					htmlGuts.append(value);
				}
				else {
					htmlGuts.append(label.getFor());
				}
			}
			htmlGuts.append("</span>");
		}
		else {
			String value = label.getValue();
			boolean boundValue = (value != null) && BindUtil.messageIsBound(value);
			if (boundValue) { // has a binding expression
				if (parentVisible && visible(label)) {
					if ((! forApply) || 
							(forApply && parentEnabled)) {
						addFormat(value);
					}
					else {
						// ensure the format counter is incremented to stay in sync with the generated edit view
						formatCounter++;
					}
				}
				else {
					// ensure the format counter is incremented to stay in sync with the generated edit view
					formatCounter++;
				}
			}
			else {
				if (parentVisible && visible(label)) {
					if ((! forApply) || 
							(forApply && parentEnabled)) {
						addBinding(label.getBinding(), false);
					}
				}
			}
			addCondition(label.getInvisibleConditionName());
		}
	}

	@Override
	public void visitLink(Link link,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			if (htmlGuts.length() > 0) {
				htmlGuts.append("&nbsp;");
			}
		}
		
		htmlGuts.append("<a href=\"");
		hrefProcessor.process(link.getReference());
		htmlGuts.append('"');
		if (visitingDataWidget) {
			appendHtmlGutsStyle(link.getPixelWidth(), null,  null, link.getInvisibleConditionName());
		}
		
		ReferenceTarget target = link.getTarget();
		if (target != null) {
			ReferenceTargetType type = target.getType();
			if (ReferenceTargetType.blankFrame.equals(type)) {
				htmlGuts.append(" target=\"_blank\"");
			}
			else if (ReferenceTargetType.namedFame.equals(type)) {
				htmlGuts.append(" target=\"").append(target.getName()).append('"');
			}
		}
			
		String value = link.getValue();
		if (value != null) {
			htmlGuts.append(">").append(value).append("</a>");
		}
		else {
			htmlGuts.append("/>");
		}
		
		if (! visitingDataWidget) {
			addFormat(htmlGuts.toString());
			htmlGuts.setLength(0);
			
			addCondition(link.getInvisibleConditionName());
		}
	}
	
	private void appendHtmlGutsStyle(Integer pixelWidth,
										Integer pixelHeight,
										HorizontalAlignment textAlignment,
										String invisibleConditionName) {
		if ((pixelWidth != null) || (pixelHeight != null) || (invisibleConditionName != null)) {
			htmlGuts.append(" style=\"");
			if (pixelWidth != null) {
				htmlGuts.append("width:").append(pixelWidth).append("px;");
			}
			if (pixelHeight != null) {
				htmlGuts.append("height:").append(pixelHeight).append("px;");
			}
			if (textAlignment != null) {
				htmlGuts.append("text-align:").append(textAlignment.toAlignmentString()).append(';');
			}
			if (invisibleConditionName != null) {
				if ("true".equals(invisibleConditionName)) {
					htmlGuts.append("display:none;");
				}
				else if (! "false".equals(invisibleConditionName)) {
					// NB - "display:true;" and "display:false;" will be replaced in addBindingsAndFormatValues()
					htmlGuts.append("display:{").append(invisibleConditionName).append("};");
				}
			}
			htmlGuts.append('"');
		}
	}
	
	@Override
	public void visitContentLink(ContentLink link, 
									boolean parentVisible,
									boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(link)) {
			if ((! forApply) || 
					(forApply && parentEnabled && (! Boolean.FALSE.equals(link.getEditable())))) {
				addBinding(link.getBinding(), true);
			}
		}
		addCondition(link.getInvisibleConditionName());
		addBinding(Bean.MODULE_KEY, false);
		addBinding(Bean.DOCUMENT_KEY, false);
		addBinding(Bean.DATA_GROUP_ID, false);
		addBinding(Bean.USER_ID, false);
	}

	@Override
	public void visitParameter(Parameter parameter,
								boolean parentVisible,
								boolean parentEnabled) {
		if (parentVisible) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addBinding(parameter.getBinding(), false);
			}
		}
	}

	@Override
	public void visitFilterParameter(FilterParameter parameter,
										boolean parentVisible,
										boolean parentEnabled) {
		visitParameter(parameter, parentVisible, parentEnabled);
	}

	@Override
	public void visitProgressBar(ProgressBar progressBar,
									boolean parentVisible,
									boolean parentEnabled) {
		if (parentVisible && visible(progressBar)) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addBinding(progressBar.getBinding(), false);
			}
		}
		addCondition(progressBar.getInvisibleConditionName());
	}

	@Override
	public void visitListGrid(ListGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		addCondition(grid.getDisabledConditionName());
		addCondition(grid.getInvisibleConditionName());
		if (parentVisible && visible(grid)) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addCondition(grid.getDisableAddConditionName());
				addCondition(grid.getDisableEditConditionName());
				addCondition(grid.getDisableZoomConditionName());
				addCondition(grid.getDisableRemoveConditionName());
				addCondition(grid.getPostRefreshConditionName());
				addBinding(grid.getSelectedIdBinding(), true);
			}
		}
	}

	@Override
	public void visitListRepeater(ListRepeater repeater,
									boolean parentVisible,
									boolean parentEnabled) {
		addCondition(repeater.getInvisibleConditionName());
		if (parentVisible && visible(repeater)) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addCondition(repeater.getPostRefreshConditionName());
			}
		}
	}

	@Override
	public void visitTreeGrid(TreeGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		addCondition(grid.getDisabledConditionName());
		addCondition(grid.getInvisibleConditionName());
		if (parentVisible && visible(grid)) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				addCondition(grid.getDisableAddConditionName());
				addCondition(grid.getDisableEditConditionName());
				addCondition(grid.getDisableZoomConditionName());
				addCondition(grid.getDisableRemoveConditionName());
				addCondition(grid.getPostRefreshConditionName());
				addBinding(grid.getSelectedIdBinding(), true);
				addBinding(grid.getRootIdBinding(), false);
			}
		}
	}

	private boolean visitingDataWidget = false;
	private boolean visitedDataWidgetHasEditableColumns = false;

	@Override
	public void visitDataGrid(DataGrid grid,
								boolean parentVisible,
								boolean parentEnabled) {
		// The grid columns are editable if the grid is editable, and either the edit function is enabled or 
		// the add function is enabled for an inline grid
		// NB The zoom operation can be disabled and not affect whether the columns are editable because 
		// the changed values are posted in the zoomed-in page, not from the grid.
		boolean dataWidgetHasEditableColumns = (! Boolean.FALSE.equals(grid.getEditable())) &&
												(evaluateConditionInOppositeSense(grid.getDisableEditConditionName()) ||
													(Boolean.TRUE.equals(grid.getInline()) && 
														evaluateConditionInOppositeSense(grid.getDisableAddConditionName())));

		visitDataWidget(grid, 
							parentVisible,
							parentEnabled,
							enabled(grid),
							dataWidgetHasEditableColumns,
							grid.getDisableAddConditionName(),
							grid.getDisableZoomConditionName(),
							grid.getDisableEditConditionName(),
							grid.getDisableRemoveConditionName(),
							grid.getSelectedIdBinding());
		addCondition(grid.getDisabledConditionName());
	}
	
	@Override
	public void visitDataRepeater(DataRepeater repeater,
									boolean parentVisible,
									boolean parentEnabled) {
		visitDataWidget(repeater, parentVisible, parentEnabled, true, true, null, null, null, null, null);
	}
	
	private void visitDataWidget(AbstractDataWidget widget,
									boolean parentVisible,
									boolean parentEnabled,
									boolean enabled,
									boolean dataWidgetHasEditableColumns,
									String disableAddConditionName,
									String disableZoomConditionName,
									String disableEditConditionName,
									String disableRemoveConditionName,
									String selectedIdBinding) {	
		htmlGuts.setLength(0);

		addCondition(widget.getInvisibleConditionName());

		// NB Allow bindings in a grid with getEditable() false through as there could be 
		// links with actions that mutate the grid data client-side ie remove implicit action
		if (parentVisible && visible(widget)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled)) {
				visitingDataWidget = true;
				visitedDataWidgetHasEditableColumns = dataWidgetHasEditableColumns;
				
				addCondition(disableAddConditionName);
				addCondition(disableZoomConditionName);
				addCondition(disableEditConditionName);
				addCondition(disableRemoveConditionName);
				addBinding(selectedIdBinding, true, true);
				
				String gridBinding = widget.getBinding();
			    TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, gridBinding);
			    Relation targetRelation = (Relation) target.getAttribute();
			    Document relatedDocument = module.getDocument(customer, targetRelation.getDocumentName());
		        currentBindings = currentBindings.putOrGetChild(gridBinding, relatedDocument);
		        
		        // Add these for polymorphic zooming on data grids
		        addBinding(Bean.MODULE_KEY, false);
		        addBinding(Bean.DOCUMENT_KEY, false);
		        
		        if (targetRelation instanceof Collection) {
			        Collection collection = (Collection) targetRelation;
			        // Only child collections have the bizOrdinal property exposed
			        if (Boolean.TRUE.equals(collection.getOrdered()) && CollectionType.child.equals(collection.getType())) {
						addBinding(Bean.ORDINAL_NAME, true);
					}
		        }
		        
				// Cater for the case where this is a grid with a lookup description representing the entire row
				List<? extends TabularColumn> gridColumns = widget.getColumns();
				if (gridColumns.size() == 1) {
					TabularColumn gridColumn = gridColumns.get(0);
					if (gridColumn instanceof DataGridBoundColumn) {
						DataGridBoundColumn boundGridColumn = (DataGridBoundColumn) gridColumn;
						if (boundGridColumn.getBinding() == null) {
							addBinding(Bean.DOCUMENT_ID, true);
							WidgetReference ref = boundGridColumn.getInputWidget();
							if (ref != null) {
								InputWidget inputWidget = ref.getWidget();
								if (inputWidget instanceof LookupDescription) {
									LookupDescription lookup = (LookupDescription) inputWidget;
									addBinding(lookup.getDescriptionBinding(), false);
								}
							}
						}
					}
				}
			}
			else {
				// grid is disabled
				visitedDataWidgetHasEditableColumns = false;
			}
		}
		else {
			// grid is invisible
			visitedDataWidgetHasEditableColumns = false;
		}
	}
	
	@Override
	public void visitedListGrid(ListGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		incrementCounter();
	}

	@Override
	public void visitedListRepeater(ListRepeater repeater,
										boolean parentVisible,
										boolean parentEnabled) {
		incrementCounter();
	}

	@Override
	public void visitedTreeGrid(TreeGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		incrementCounter();
	}

	@Override
	public void visitedDataGrid(DataGrid grid,
									boolean parentVisible,
									boolean parentEnabled) {
		incrementCounter();
		visitedDataWidget();
	}

	@Override
	public void visitedDataRepeater(DataRepeater repeater,
										boolean parentVisible,
										boolean parentEnabled) {
		incrementCounter();
		visitedDataWidget();
	}

	// keep this in sync with the generated edit views
	private void incrementCounter() {
		if (ViewType.create.toString().equals(view.getName())) {
			createIdCounter++;
		}
		else {
			editIdCounter++;
		}
	}
	
	private void visitedDataWidget() {
		if (visitingDataWidget) {
		    currentBindings = currentBindings.getParent();
		}
		visitingDataWidget = false;
		htmlGuts.setLength(0);
	}
	
	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
		if (parentVisible) {
			if ((! forApply) || 
					(forApply && 
						parentEnabled && 
						visitedDataWidgetHasEditableColumns && 
						(! Boolean.FALSE.equals(column.getEditable())))) {
				addBinding(column.getBinding(), true);
			}
		}
	}

	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column,
											boolean parentVisible,
											boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible,
												boolean parentEnabled) {
		htmlGuts.setLength(0);
	}

	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column,
												boolean parentVisible,
												boolean parentEnabled) {
		addFormat(UtilImpl.processStringValue(htmlGuts.toString()));
	}

	@Override
	public void visitCheckBox(CheckBox checkBox,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(checkBox)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(checkBox))) {
				addBinding(checkBox.getBinding(), true);
			}
		}
		addCondition(checkBox.getDisabledConditionName());
		addCondition(checkBox.getInvisibleConditionName());
	}

	@Override
	public void visitedCheckBox(CheckBox checkBox,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitCheckMembership(CheckMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		addCondition(membership.getDisabledConditionName());
		addCondition(membership.getInvisibleConditionName());

		if (parentVisible && visible(membership)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(membership))) {
				String binding = membership.getBinding();
				if (binding != null) {
				    TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				    Document referenceDocument = module.getDocument(customer, ((Reference) target.getAttribute()).getDocumentName());
				    currentBindings = currentBindings.putOrGetChild(binding, referenceDocument);
				}
	
				addBinding(Bean.DOCUMENT_ID, true);
				addBinding(Bean.BIZ_KEY, false);

				if (binding != null) {
					currentBindings = currentBindings.getParent();
				}
			}
		}
	}

	@Override
	public void visitedCheckMembership(CheckMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitColourPicker(ColourPicker colour,
									boolean parentVisible,
									boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(colour)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(colour))) {
				addBinding(colour.getBinding(), true);
			}
		}
		addCondition(colour.getDisabledConditionName());
		addCondition(colour.getInvisibleConditionName());
	}

	@Override
	public void visitedColourPicker(ColourPicker colour,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitCombo(Combo combo,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			if (parentVisible) {
				if ((! forApply) || 
					(forApply && parentEnabled)) {
					    StringBuilder fullBinding = new StringBuilder(64);
					    fullBinding.append(currentBindings.getBindingPrefix()).append('.').append(combo.getBinding());
					    putVariantAndDynamicDomainValuesInValueMaps(fullBinding.toString());
				}
			}
			return;
		}

		if (parentVisible && visible(combo)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(combo))) {
				String binding = combo.getBinding();
				addBinding(binding, true);
				
				putVariantAndDynamicDomainValuesInValueMaps(binding);
			}
		}
		addCondition(combo.getDisabledConditionName());
		addCondition(combo.getInvisibleConditionName());
	}

	@Override
	public void visitedCombo(Combo combo,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitRichText(RichText text,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(text)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(text))) {
				addBinding(text.getBinding(), true);
			}
		}
		addCondition(text.getDisabledConditionName());
		addCondition(text.getInvisibleConditionName());
	}

	@Override
	public void visitedRichText(RichText richText,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitHTML(HTML html,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}
		if (parentVisible && visible(html)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(html))) {
				addBinding(html.getBinding(), true);
			}
		}
		addCondition(html.getDisabledConditionName());
		addCondition(html.getInvisibleConditionName());
	}

	@Override
	public void visitListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		addCondition(membership.getDisabledConditionName());
		addCondition(membership.getInvisibleConditionName());

		if (parentVisible && visible(membership)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(membership))) {
				String binding = membership.getBinding();
				if (binding != null) {
				    TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				    Document referenceDocument = module.getDocument(customer, ((Relation) target.getAttribute()).getDocumentName());
				    currentBindings = currentBindings.putOrGetChild(binding, referenceDocument);
				}
	
				addBinding(Bean.DOCUMENT_ID, true);
				addBinding(Bean.BIZ_KEY, false);

				putVariantAndDynamicDomainValuesInValueMaps(binding);
				
				if (binding != null) {
					currentBindings = currentBindings.getParent();
				}
			}
		}
	}

	@Override
	public void visitedListMembership(ListMembership membership,
										boolean parentVisible,
										boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitComparison(Comparison comparison,
									boolean parentVisible,
									boolean parentEnabled) {
		addCondition(comparison.getDisabledConditionName());
		addCondition(comparison.getInvisibleConditionName());

		if (parentVisible && visible(comparison)) {
			String referenceName = comparison.getBinding();
			Reference reference = document.getReferenceByName(referenceName);
			Document referenceDocument = module.getDocument(customer, reference.getDocumentName());

			try {
				AbstractRepository repository = AbstractRepository.get();
				ComparisonModel<Bean, Bean> model = repository.getComparisonModel(customer, 
																					document,
																					comparison.getModelName(),
																					true);
				model.setBean(bean);
				ComparisonComposite root = model.getComparisonComposite((Bean) BindUtil.get(bean, referenceName));
				if (! forApply) {
					comparisons.put(referenceName, 
										new ComparisonJSONManipulator((UserImpl) user, 
																		customer,
																		root).toJSONStructure());
				}
				else if (forApply && 
							(! Boolean.FALSE.equals(comparison.getEditable())) && 
							parentEnabled && 
							enabled(comparison)) {
			        currentBindings = currentBindings.putOrGetChild(referenceName, referenceDocument);
			        addComparisonBindingsForApply(root, referenceDocument);
			        currentBindings = currentBindings.getParent();
				}
			}
			catch (Exception e) {
				throw new MetaDataException("Could not populate the comparison editor [" + referenceName + ']', e);
			}
		}
	}
	
	private void addComparisonBindingsForApply(ComparisonComposite node, Document nodeDocument) {
        String nodeReferenceName = node.getReferenceName();
        if (nodeReferenceName != null) {
            currentBindings = currentBindings.putOrGetChild(nodeReferenceName, nodeDocument);
        }

        for (ComparisonProperty property : node.getProperties()) {
            currentBindings.putBinding(property.getName(), true);
        }
        
        for (ComparisonComposite child : node.getChildren()) {
        	TargetMetaData target = Binder.getMetaDataForBinding(customer, module, nodeDocument, child.getReferenceName());
        	Relation childRelation = (Relation) target.getAttribute();
            Document childDocument = module.getDocument(customer, childRelation.getDocumentName());
            addComparisonBindingsForApply(child, childDocument);
        }
        
        if (nodeReferenceName != null) {
            currentBindings = currentBindings.getParent();
        }
	}

	@Override
	public void visitLookupDescription(LookupDescription lookup,
										boolean parentVisible,
										boolean parentEnabled) {
		if (visitingDataWidget) {
			// Can be no lookup binding if the lookup is in a data grid and represents the entire data grid row
			String lookupBinding = lookup.getBinding();
			if ((! forApply) && (lookupBinding != null)) {
				StringBuilder bindingBuilder = new StringBuilder(64);
				bindingBuilder.append(lookupBinding).append('.').append(lookup.getDescriptionBinding());
				addBinding(bindingBuilder.toString(), true);
			}
			return;
		}

		if (parentVisible && visible(lookup)) {
			if ((! forApply) || 
					// Although LookupDescription implements Editable,
					// editable here means there is no split button displayed;
					// the combo still auto-completes and so the data still needs to be applied.
					(forApply && parentEnabled && enabled(lookup))) {
				// LookupDescription binding can be null when the lookup is in a 
				// data grid and the lookup selects the entire row
				String binding = lookup.getBinding();
				if (binding == null) { 
					addBinding(lookup.getDescriptionBinding(), false);
				}
				else { // not in a data grid
					addBinding(binding, true);
					
					// only include the description binding if we are fetching
					if (! forApply) {
						StringBuilder bindingBuilder = new StringBuilder(64);
						bindingBuilder.append(binding).append('.').append(lookup.getDescriptionBinding());
						addBinding(bindingBuilder.toString(), false);
					}
				}
			}
		}
		
		addCondition(lookup.getDisabledConditionName());
		addCondition(lookup.getInvisibleConditionName());
		addCondition(lookup.getDisablePickConditionName());
		addCondition(lookup.getDisableEditConditionName());
		addCondition(lookup.getDisableAddConditionName());
		addCondition(lookup.getDisableClearConditionName());
	}
	
	@Override
	public void visitedLookupDescription(LookupDescription lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(lookup)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(lookup))) {
				addBinding(new StringBuilder(32).append(lookup.getBinding()).append('.').append(Bean.DOCUMENT_ID).toString(), true);
			}
		}
		addCondition(lookup.getDisabledConditionName());
		addCondition(lookup.getInvisibleConditionName());
		addCondition(lookup.getDisablePickConditionName());
		addCondition(lookup.getDisableEditConditionName());
		addCondition(lookup.getDisableAddConditionName());
		addCondition(lookup.getDisableClearConditionName());
	}

	@Override
	public void visitedLookup(Lookup lookup,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitPassword(Password password, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible & visible(password)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(password))) {
				addBinding(password.getBinding(), true);
			}
		}
		addCondition(password.getDisabledConditionName());
		addCondition(password.getInvisibleConditionName());
	}

	@Override
	public void visitedPassword(Password password,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitRadio(Radio radio,
							boolean parentVisible,
							boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}
		if (parentVisible && visible(radio)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(radio))) {
				addBinding(radio.getBinding(), true);
			}
		}
		addCondition(radio.getDisabledConditionName());
		addCondition(radio.getInvisibleConditionName());
	}

	@Override
	public void visitedRadio(Radio radio,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSlider(Slider slider, 
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(slider)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(slider))) {
				addBinding(slider.getBinding(), true);
			}
		}
		addCondition(slider.getDisabledConditionName());
		addCondition(slider.getInvisibleConditionName());
	}

	@Override
	public void visitedSlider(Slider slider,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitSpinner(Spinner spinner,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(spinner)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(spinner))) {
				addBinding(spinner.getBinding(), true);
			}
		}
		addCondition(spinner.getDisabledConditionName());
		addCondition(spinner.getInvisibleConditionName());
	}

	@Override
	public void visitedSpinner(Spinner spinner,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitTextArea(TextArea text,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(text)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(text) && (! Boolean.FALSE.equals(text.getEditable())))) {
				addBinding(text.getBinding(), true);
			}
		}
		addCondition(text.getDisabledConditionName());
		addCondition(text.getInvisibleConditionName());
	}

	@Override
	public void visitedTextArea(TextArea text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitTextField(TextField text,
								boolean parentVisible,
								boolean parentEnabled) {
		if (visitingDataWidget) {
			return;
		}

		if (parentVisible && visible(text)) {
			if ((! forApply) || 
					(forApply && parentEnabled && enabled(text) && (! Boolean.FALSE.equals(text.getEditable())))) {
				addBinding(text.getBinding(), true);
			}
		}
		addCondition(text.getDisabledConditionName());
		addCondition(text.getInvisibleConditionName());
	}

	@Override
	public void visitedTextField(TextField text,
									boolean parentVisible,
									boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		if (parentVisible) {
			if ((! forApply) || 
					(forApply && parentEnabled)) {
				for (InjectBinding binding : inject.getBindings()) {
					addBinding(binding.getBinding(), Boolean.FALSE.equals(binding.getReadOnly()));
				}
			}
		}
	}

	@Override
	public void visitedView() {
		// do nothing
	}

	@Override
	public void visitedVBox(VBox vbox,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	@Override
	public void visitedHBox(HBox hbox,
								boolean parentVisible,
								boolean parentEnabled) {
		// do nothing
	}

	private void visitActionInternal(Action action) {
		addCondition(action.getDisabledConditionName());
		addCondition(action.getInvisibleConditionName());
	}
	
	@Override
	public void visitCustomAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitAddAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitRemoveAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitZoomOutAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitNavigateAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitOKAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitSaveAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitCancelAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitDeleteAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitReportAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitBizExportAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitBizImportAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitDownloadAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitUploadAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitNewAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitEditAction(ActionImpl action) {
		visitActionInternal(action);
	}

	@Override
	public void visitOnChangedEventHandler(Changeable changeable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnChangedEventHandler(Changeable changeable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnFocusEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnBlurEventHandler(Focusable blurable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnAddedEventHandler(Addable addable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnEditedEventHandler(Editable editable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnEditedEventHandler(Editable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnRemovedEventHandler(Removable removable,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnRemovedEventHandler(Removable removable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnSelectedEventHandler(Selectable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnSelectedEventHandler(Selectable editable,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnPickedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnPickedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitOnClearedEventHandler(Lookup lookup,
											boolean parentVisible,
											boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitedOnClearedEventHandler(Lookup lookup,
												boolean parentVisible,
												boolean parentEnabled) {
		// nothing to do here
	}

	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender,
											EventSource source,
											boolean parentVisible,
											boolean parentEnabled) {
		// no properties here
	}

	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server,
													boolean parentVisible,
													boolean parentEnabled) {
		// no properties here
	}

	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		addCondition(setDisabled.getDisabledConditionName());
		// we add this binding as the widget could be enabled client-side and we'd need to let the value through
		addBinding(setDisabled.getBinding(), true);
	}

	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled,
												boolean parentVisible,
												boolean parentEnabled) {
		// we add this binding as the widget could be enabled client-side and we'd need to let the value through
		addBinding(toggleDisabled.getBinding(), true);
	}

	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility,
													boolean parentVisible,
													boolean parentEnabled) {
		// we add this binding as the widget could be visible client-side and we'd need to let the value through
		addBinding(toggleVisibility.getBinding(), true);
	}


	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible,
												boolean parentVisible,
												boolean parentEnabled) {
		addCondition(setInvisible.getInvisibleConditionName());
		// we add this binding as the widget could be visible client-side and we'd need to let the value through
		addBinding(setInvisible.getBinding(), true);
	}
}
