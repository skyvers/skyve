package org.skyve.impl.generate;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Geometry;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class ViewGenerator {
// Revert the responsive gutter centred layout
//	private static final Integer ONE = Integer.valueOf(1);
//	private static final Integer TWO = Integer.valueOf(2);
//	private static final Integer THREE = Integer.valueOf(3);
	private static final Integer FOUR = Integer.valueOf(4);
//	private static final Integer SIX = Integer.valueOf(6);
//	private static final Integer EIGHT = Integer.valueOf(8);
	private static final Integer TWELVE = Integer.valueOf(12);
	private static final Integer SIXTY = Integer.valueOf(60);
	
	private ProvidedRepository repository;
	
	public ViewGenerator(ProvidedRepository repository) {
		this.repository = repository;
	}

	public ViewImpl generate(Customer customer, Document document, String viewName) {
		ViewImpl result = null;

		Module module = customer.getModule(document.getOwningModuleName());

		if (ViewType.list.toString().equals(viewName)) {
			QueryDefinition defaultQuery = module.getDocumentDefaultQuery(customer, document.getName());
			result = generateListView(customer, document, defaultQuery, null);
		}
		else if (ViewType.edit.toString().equals(viewName)) {
			// NB Default to side layout
			result = generateEditView(customer, module, document, module.getFormLabelLayout() != FormLabelLayout.top);
		}
		else {
			throw new IllegalArgumentException("ViewGenerator : Cannot generate a view of type " + viewName);
		}

		return result;
	}

	@SuppressWarnings("unused")
	public static ViewImpl generateListView(Customer customer, Document document, QueryDefinition query, String description) {
		ViewImpl result = new ViewImpl();
		result.setName(ViewType.list.toString());
		StringBuilder title = new StringBuilder(64);
		String finalDescription = description;
		if (finalDescription == null) {
			finalDescription = document.getLocalisedDescription();
		}
		if (finalDescription == null) {
			finalDescription = document.getLocalisedPluralAlias();
		}
		if (finalDescription != null) {
			title.append(finalDescription);
		}
		result.setTitle(title.toString());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		result.putAction(action);

		return result;
	}

	private static class Detail {
		String title;
		MetaData widget;
	}
	
	private ViewImpl generateEditView(Customer customer, Module module, Document document, boolean formLabelSideLayout) {
		ViewImpl result = new ViewImpl();
		result.setName(ViewType.edit.toString());

		result.setTitle(document.getLocalisedSingularAlias());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		result.putAction(action);

		// Add any actions that have privileges
		for (String actionName : ((DocumentImpl) document).getDefinedActionNames()) {
			action = new ActionImpl();
			try {
				repository.getServerSideAction(customer, document, actionName, false);
			}
			catch (@SuppressWarnings("unused") Exception e) {
				try {
					repository.getUploadAction(customer, document, actionName, false);
					action.setImplicitName(ImplicitActionName.Upload);
				}
				catch (@SuppressWarnings("unused") Exception e1) {
					try {
						repository.getDownloadAction(customer, document, actionName, false);
						action.setImplicitName(ImplicitActionName.Download);
					}
					catch (@SuppressWarnings("unused") Exception e2) {
						try {
							repository.getBizExportAction(customer, document, actionName, false);
							action.setImplicitName(ImplicitActionName.BizExport);
						}
						catch (@SuppressWarnings("unused") Exception e3) {
							try {
								repository.getBizImportAction(customer, document, actionName, false);
								action.setImplicitName(ImplicitActionName.BizImport);
							}
							catch (@SuppressWarnings("unused") Exception e4) {
								throw new MetaDataException(actionName + " cannot be found");
							}
						}
					}
				}
			}

			action.setResourceName(actionName);
			action.setDisplayName(actionName);
			result.putAction(action);
		}

		// <hbox shrinkWrap="height">
// Revert the responsive gutter centred layout
//		HBox hbox = new HBox();
//		hbox.setShrinkWrap(ShrinkWrap.height);
//		List<MetaData> hboxGuts = hbox.getContained();
//		hboxGuts.add(responsiveGutter());
		
		// <form border="true" responsiveWidth="8" sm="12" lg="6" xl="4">
		Form form = new Form();
		form.setBorder(Boolean.TRUE);
// Revert the responsive gutter centred layout
		form.setPercentageWidth(SIXTY);
		form.setResponsiveWidth(TWELVE);
//		form.setResponsiveWidth(EIGHT);
//		form.setSm(TWELVE);
//		form.setLg(SIX);
//		form.setXl(FOUR);
		if (formLabelSideLayout) {
			FormColumn column = new FormColumn();
			column.setResponsiveWidth(FOUR);
			form.getColumns().add(column);
		}
		form.getColumns().add(new FormColumn());
		
		List<Detail> details = new ArrayList<>();

		processAttributes(customer, module, document, form, details, null);

// Revert the responsive gutter centred layout
//		hboxGuts.add(form);
//		hboxGuts.add(responsiveGutter());

		// make a tabbed view if more than 1 detail widget or there is 1 detail widget and more than 5 form fields
		int numberOfDetailWidgets = details.size();
		if ((numberOfDetailWidgets > 1) || 
				((numberOfDetailWidgets == 1) && (form.getRows().size() > 5))) {
			TabPane tabPane = new TabPane();
			Tab tab = null;
			if (! form.getRows().isEmpty()) {
				tab = new Tab();
				tab.setTitle("General");
// Revert the responsive gutter centred layout
				tab.getContained().add(form);
//				tab.getContained().add(hbox);
				tabPane.getTabs().add(tab);
			}
			
			for (Detail detail : details) {
				tab = new Tab();
				tab.setTitle(detail.title);
				MetaData detailWidget = detail.widget;
				if (detailWidget instanceof AbstractDataWidget) {
					AbstractDataWidget adw = (AbstractDataWidget) detailWidget;
					adw.setTitle(null);
				}
				tab.getContained().add(detailWidget);
				tabPane.getTabs().add(tab);
			}
			result.getContained().add(tabPane);
		}
		else {
			if (! form.getRows().isEmpty()) {
// Revert the responsive gutter centred layout
				result.getContained().add(form);
//				result.getContained().add(hbox);
			}

			for (Detail detail : details) {
				result.getContained().add(detail.widget);
			}
		}

		return result;
	}

/* Revert the responsive gutter centred layout
	// <vbox responsiveWidth="2" sm="12" lg="3" xl="4" pixelHeight="1" />
	private static VBox responsiveGutter() {
		VBox result = new VBox();
		result.setResponsiveWidth(TWO);
		result.setSm(TWELVE);
		result.setLg(THREE);
		result.setXl(FOUR);
		result.setPixelHeight(ONE);
		return result;
	}
*/
	private void processAttributes(Customer customer, 
									Module module, 
									Document document,
									Form form,
									List<Detail> details,
									String bindingPrefix) {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			processAttributes(customer, module, baseDocument, form, details, bindingPrefix);
		}

		Bizlet<Bean> bizlet = null;
		String moduleName = module.getName();
		String documentName = document.getName();
		
		for (Attribute attribute : document.getAttributes()) {
			if (! attribute.isDeprecated()) {
				String binding = (bindingPrefix == null) ?
									attribute.getName() :
									bindingPrefix + attribute.getName();

				DomainType domainType = attribute.getDomainType();
				if (module.isPrototype() &&
						attribute.isRequired() &&
						(DomainType.constant.equals(domainType) ||
						AttributeType.enumeration.equals(attribute.getAttributeType()))) {
					List<DomainValue> domainValues = null;
					try {
						if (bizlet == null) {
							bizlet = repository.getBizlet(customer, document, false);
						}
						domainValues = customer.getConstantDomainValues(bizlet, moduleName, documentName, attribute);
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// nothing to do here - who cares
					}
					MetaData widget = null;
					if ((domainValues != null) && (domainValues.size() <= 3)) {
						Radio radio = new Radio();
						radio.setBinding(binding);
						widget = radio;
					}
					else {
						DefaultWidget defaultWidget = new DefaultWidget();
						defaultWidget.setBinding(binding);
						widget = defaultWidget;
					}

					FormItem item = new FormItem();
					item.setWidget(widget);
					FormRow row = new FormRow();
					row.getItems().add(item);
					form.getRows().add(row);
				}
				else if (attribute instanceof Collection) {
					Collection collection = (Collection) attribute;
					Document detailDocument = module.getDocument(customer, collection.getDocumentName());
	
					List<String> propertyNames = new ArrayList<>();
					populatePropertyNames(customer, module, detailDocument, propertyNames);
					
					if (collection.getDomainType() == null) {
						Detail detail = new Detail();
						detail.title = attribute.getLocalisedDisplayName();
						detail.widget = generateDataGrid(collection.getType(), 
															customer,
															module,
															detailDocument, 
															binding, 
															propertyNames);
						details.add(detail);
					}
					else {
						Detail detail = new Detail();
						detail.title = attribute.getLocalisedDisplayName();
						ListMembership membership = new ListMembership();
						membership.setBinding(binding);
						membership.setCandidatesHeading("Candidates");
						membership.setMembersHeading("Members");
						detail.widget = membership;
						details.add(detail);
					}
				}
				else if ((attribute instanceof Inverse) && 
							InverseCardinality.many.equals(((Inverse) attribute).getCardinality())) {
					Inverse inverse = (Inverse) attribute;
					Document detailDocument = module.getDocument(customer, inverse.getDocumentName());
	
					List<String> propertyNames = new ArrayList<>();
					populatePropertyNames(customer, module, detailDocument, propertyNames);
					
					Detail detail = new Detail();
					detail.title = attribute.getLocalisedDisplayName();
					detail.widget = generateDataGrid(CollectionType.composition,
														customer,
														module,
														detailDocument, 
														binding, 
														propertyNames);
					details.add(detail);
				}
				else if (attribute instanceof Association) {
					Association association = (Association) attribute;
					Document associationDocument = module.getDocument(customer, association.getDocumentName());
					if (AssociationType.embedded.equals(association.getType())) {
						Module associationModule = customer.getModule(associationDocument.getOwningModuleName());
						processAttributes(customer, associationModule, associationDocument, form, details, binding + '.');
					}
					else {
						MetaData metaData = null;
						// Use a bizKey text field when there is no domain values defined and the association document is not persistent
						if ((domainType == null) && (! associationDocument.isPersistable())) {
							TextField widget = new TextField();
							widget.setBinding(Binder.createCompoundBinding(binding, Bean.BIZ_KEY));
							metaData = widget;
						}
						else {
							DefaultWidget widget = new DefaultWidget();
							widget.setBinding(binding);
							metaData = widget;
						}
						FormItem item = new FormItem();
						item.setWidget(metaData);
						FormRow row = new FormRow();
						row.getItems().add(item);
						form.getRows().add(row);
					}
				}
				else if (module.isPrototype() && (attribute instanceof Geometry)) {
					FormItem item = new FormItem();
					GeometryMap widget = new GeometryMap();
					widget.setBinding(binding);
					item.setWidget(widget);
					FormRow row = new FormRow();
					row.getItems().add(item);
					form.getRows().add(row);
				}
				else if (module.isPrototype() && (attribute instanceof Content)) {
					FormItem item = new FormItem();
					ContentImage widget = new ContentImage();
					widget.setBinding(binding);
					item.setWidget(widget);
					FormRow row = new FormRow();
					row.getItems().add(item);
					form.getRows().add(row);
				}
				else { // field or inverseOne
					FormItem item = new FormItem();
					DefaultWidget widget = new DefaultWidget();
					widget.setBinding(binding);
					item.setWidget(widget);
					FormRow row = new FormRow();
					row.getItems().add(item);
					form.getRows().add(row);
				}
			}
		}
	}
	
	private static void populatePropertyNames(Customer customer,
												Module module,
												Document document,
												List<String> propertyNames) {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			String inheritedDocumentName = inherits.getDocumentName();
			Document inheritedDocument = module.getDocument(customer, inheritedDocumentName);
			populatePropertyNames(customer, module, inheritedDocument, propertyNames);
		}
		for (Attribute detailAttribute : document.getAttributes()) {
			if ((! (detailAttribute instanceof Collection)) && (! (detailAttribute instanceof Inverse)) && // only scalars
					(! detailAttribute.getName().equals(Bean.BIZ_KEY))) {
				propertyNames.add(detailAttribute.getName());
			}
		}
	}

	private static DataGrid generateDataGrid(CollectionType collectionType,
												Customer customer,
												Module module,
												Document document,
												String dataGridBinding,
												List<String> propertyNames) {
		DataGrid result = new DataGrid();
		if (dataGridBinding != null) {
			result.setBinding(dataGridBinding);
			result.setTitle(document.getLocalisedPluralAlias());
		}

		if (CollectionType.aggregation.equals(collectionType)) {
			DataGridBoundColumn column = new DataGridBoundColumn();
			column.setBinding(null); // no column binding

			LookupDescription lookup = new LookupDescription();
			lookup.setDescriptionBinding(Bean.BIZ_KEY);
			WidgetReference lookupRef = new WidgetReference();
			lookupRef.setWidget(lookup);
			column.setInputWidget(lookupRef);

			result.getColumns().add(column);
		}
		else {
			for (String propertyName : propertyNames) {
				TargetMetaData target = Binder.getMetaDataForBinding(customer, module, document, propertyName);
				Attribute attribute = target.getAttribute();
				DataGridBoundColumn column = new DataGridBoundColumn();

				DomainType domainType = attribute.getDomainType();
				// Set this field as non-editable coz the default widget (lookup description) 
				// cannot work with dynamic domain values in a list
				if (DomainType.dynamic.equals(domainType)) {
					column.setEditable(Boolean.FALSE);
					if ((attribute instanceof Association) || (attribute instanceof InverseOne)) {
						column.setBinding(Binder.createCompoundBinding(propertyName, Bean.BIZ_KEY));
					}
				}
				// Set this field as non-editable coz the default widget (lookup description) 
				// cannot query the document as its either an embedded association or not persistent
				else if (attribute instanceof Association) {
					Association association = (Association) attribute;
					Document associationDocument = module.getDocument(customer, association.getDocumentName());
					if (AssociationType.embedded.equals(association.getType()) || // embedded
							(! associationDocument.isPersistable())) { // not persistent document
						column.setBinding(Binder.createCompoundBinding(propertyName, Bean.BIZ_KEY));
						column.setEditable(Boolean.FALSE);
					}
					else {
						column.setBinding(propertyName);
					}
				}
				else {
					column.setBinding(propertyName);
				}

				result.getColumns().add(column);
			}
		}
		
		if (result.getColumns().isEmpty()) {
			DataGridBoundColumn column = new DataGridBoundColumn();
			column.setBinding(Bean.BIZ_KEY);
			column.setEditable(Boolean.FALSE);
			result.getColumns().add(column);
		}

		return result;
	}
	
	public String generateEditViewXML(Customer customer,
												Document document,
												boolean customerOverridden,
												boolean uxuiOverridden) {
		ViewImpl view = generate(customer, document, ViewType.edit.toString());
		
		ViewMetaData repositoryView = new ViewMetaData();
		repositoryView.setName(ViewType.edit.toString());
		repositoryView.setTitle(view.getTitle());

		repositoryView.getContained().addAll(view.getContained());
		Actions actions = new Actions();
		for (Action action : view.getActions()) {
			actions.getActions().add(((ActionImpl) action).toRepositoryAction());
		}
		repositoryView.setActions(actions);

		return XMLMetaData.marshalView(repositoryView, customerOverridden, uxuiOverridden);
	}
	

	private void writeEditView(String srcPath,
								Module module,
								Document document,
								Customer customer,
								boolean customerOverridden,
								String uxui)
	throws IOException {
		StringBuilder filePath = new StringBuilder(64);
		filePath.append(srcPath);
		if (customerOverridden) {
			filePath.append("customers/").append(customer.getName()).append("/modules/");
		}
		else {
			filePath.append("modules/");
		}
		filePath.append(module.getName()).append('/').append(document.getName()).append("/views/");
		if (uxui != null) {
			filePath.append(uxui).append('/');
		}
		File file = new File(filePath.toString());
		file.mkdirs();
		filePath.append("generatedEdit.xml");
		file = new File(filePath.toString());
		UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
		try (PrintWriter out = new PrintWriter(file)) {
			out.println(generateEditViewXML(customer, document, customerOverridden, uxui != null));
			out.flush();
		}
		UtilImpl.LOGGER.info("Remember to rename this to 'edit.xml' to make this view active.");
	}

	public static HorizontalAlignment determineDefaultColumnAlignment(AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType) || 
				AttributeType.dateTime.equals(attributeType) ||
				AttributeType.time.equals(attributeType) || 
				AttributeType.timestamp.equals(attributeType) ||
				AttributeType.decimal2.equals(attributeType) || 
				AttributeType.decimal5.equals(attributeType) ||
				AttributeType.decimal10.equals(attributeType) || 
				AttributeType.integer.equals(attributeType) ||
				AttributeType.longInteger.equals(attributeType)) {
			return HorizontalAlignment.right;
		}
		if (AttributeType.bool.equals(attributeType) || 
				AttributeType.content.equals(attributeType) || 
				AttributeType.image.equals(attributeType)) {
			return HorizontalAlignment.centre;
		}
		return HorizontalAlignment.left;
	}
	
	public static Integer determineDefaultColumnWidth(AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType)) {
			return Integer.valueOf(110);
		}
		if (AttributeType.dateTime.equals(attributeType)) {
			return Integer.valueOf(130);
		}
		if (AttributeType.time.equals(attributeType)) {
			return Integer.valueOf(80);
		}
		if (AttributeType.timestamp.equals(attributeType)) {
			return Integer.valueOf(140);
		}
		if (AttributeType.bool.equals(attributeType)) {
			return Integer.valueOf(75);
		}

		return null;
	}

	public static void main(String[] args) throws Exception {
		String srcPath = null;
		String customerName = null;
		String moduleName = null;
		String documentName = null;
		String uxui = null;
		boolean customerOverridden = false;
		boolean validArgs = true;

		if (args.length >= 5) {
			srcPath = args[0];
			customerName = args[1];
			moduleName = UtilImpl.processStringValue(args[2]);
			documentName = UtilImpl.processStringValue(args[3]);
			customerOverridden = Boolean.parseBoolean(args[4]);
			if (args.length == 6) {
				uxui = args[5];
			}
			if (args.length > 6) {
				validArgs = false;
			}
		}
		else {
			validArgs = false;
		}
		if (! validArgs) {
			System.err.println("Usage: org.skyve.impl.generate.ViewGenerator sourcePath (usually \"src/skyve/\") customerName moduleName documentName customerOverridden (boolean) uxui (optional)");
			System.exit(1);
		}

		
		ProvidedRepository repository = new LocalDesignRepository();
		Customer customer = repository.getCustomer(customerName);

		// If the module and/or document was not specified, we will just generate all edit views.
		if ((moduleName == null) || (documentName == null)) {
			for (Module module : customer.getModules()) {
				for (Map.Entry<String, Module.DocumentRef> entry : module.getDocumentRefs().entrySet()) {
					Module.DocumentRef documentRef = entry.getValue();
					if (documentRef.getOwningModuleName().equals(module.getName())) {
						Document document = module.getDocument(customer, entry.getKey());
						try {
							new ViewGenerator(repository).writeEditView(srcPath, module, document, customer, customerOverridden, uxui);
						}
						catch (Exception e) {
							UtilImpl.LOGGER.warning(String.format("Failed to generate edit view for %s.%s, %s.",
									module.getName(), document.getName(), e.getMessage()));
						}
					}
				}
			}
		}
		else {
			Module module = repository.getModule(customer, moduleName);
			Document document = repository.getDocument(customer, module, documentName);
			new ViewGenerator(repository).writeEditView(srcPath, module, document, customer, customerOverridden, uxui);
		}
	}
}
