package org.skyve.wildcat.generate;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.model.document.DocumentImpl;
import org.skyve.wildcat.metadata.model.document.Inverse;
import org.skyve.wildcat.metadata.model.document.field.Field;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.LocalDesignRepository;
import org.skyve.wildcat.metadata.repository.view.ViewMetaData;
import org.skyve.wildcat.metadata.view.ActionImpl;
import org.skyve.wildcat.metadata.view.HorizontalAlignment;
import org.skyve.wildcat.metadata.view.ViewImpl;
import org.skyve.wildcat.metadata.view.WidgetReference;
import org.skyve.wildcat.metadata.view.container.Tab;
import org.skyve.wildcat.metadata.view.container.TabPane;
import org.skyve.wildcat.metadata.view.container.form.Form;
import org.skyve.wildcat.metadata.view.container.form.FormColumn;
import org.skyve.wildcat.metadata.view.container.form.FormItem;
import org.skyve.wildcat.metadata.view.container.form.FormRow;
import org.skyve.wildcat.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.wildcat.metadata.view.widget.bound.input.ListMembership;
import org.skyve.wildcat.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.wildcat.metadata.view.widget.bound.input.TextField;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickList;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.PickListColumn;
import org.skyve.wildcat.metadata.view.widget.bound.tabular.TabularWidget;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

public class ViewGenerator {
	private static final Integer TWENTY = new Integer(20);
	
	private ViewGenerator() {
		// do nothing
	}

	public static ViewImpl generate(Customer customer, Document document, ViewType type) 
	throws MetaDataException {
		ViewImpl result = null;

		Module module = customer.getModule(document.getOwningModuleName());

		if (type == ViewType.list) {
			Query defaultQuery = module.getDocumentDefaultQuery(customer, document.getName());
			result = generateListView(customer, document, defaultQuery, null);
		}
		else if (type == ViewType.edit) {
			result = generateEditView(customer, module, document);
		}
		else {
			throw new IllegalArgumentException("ViewGenerator : Cannot generate a view of type " + type);
		}

		return result;
	}

	public static ViewImpl generatePickView(Customer customer,
												Module module,
												Document document,
												Reference reference,
												List<Bean> beans,
												String binding)
	throws MetaDataException {
		ViewImpl result = new ViewImpl();
		result.setType(ViewType.pick);
		result.setTitle("Pick a " + document.getSingularAlias());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.Cancel);
		result.putAction(action);

		PickList pickList = new PickList();
		pickList.setPickAssociationBinding(binding);

		if (beans.size() > 0) {
			Query query = module.getQuery(reference.getQueryName());
			for (QueryColumn queryColumn : query.getColumns()) {
				if (! queryColumn.isProjected()) {
					continue;
				}

				String columnBinding = queryColumn.getBinding();
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																		module, 
																		document,
																		columnBinding);
				Attribute attribute = target.getAttribute();
				if ((! columnBinding.equals(Bean.DOCUMENT_ID)) && 
						(! columnBinding.equals(Bean.CUSTOMER_NAME)) &&
						(! columnBinding.equals(PersistentBean.LOCK_NAME)) && 
						(attribute instanceof Field)) {
					PickListColumn column = new PickListColumn();
					column.setTitle(attribute.getDisplayName());
					// check if this column is part of the bindings
					// that is, it's a description field which is copied to the source document
					// if (pickBindings.contains(columnName))
					// {
					// column.setPickBinding(pickBindingPrefix + columnName);
					column.setPickBinding(columnBinding);
					// }
					column.setBinding(columnBinding);

					pickList.getColumns().add(column);
				}
			}
		}
		else {
			PickListColumn column = new PickListColumn();
			column.setTitle("No Data Found...");
			column.setAlignment(HorizontalAlignment.left);

			pickList.getColumns().add(column);
		}

		result.getContained().add(pickList);

		return result;
	}

	@SuppressWarnings("unused")
	public static ViewImpl generateListView(Customer customer, Document document, Query query, String description) {
		ViewImpl result = new ViewImpl();
		result.setType(ViewType.list);
		StringBuilder title = new StringBuilder(64);
		String finalDescription = description;
		if (finalDescription == null) {
			finalDescription = document.getShortDescription();
		}
		if (finalDescription == null) {
			finalDescription = document.getPluralAlias();
		}
		if (finalDescription != null) {
			title.append(finalDescription);
		}
		result.setTitle(title.toString());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		result.putAction(action);

		/*
		 * Table table = generateTable(customer, document, null, query);
		 * result.getContained().add(table);
		 * String modelName = model.getName();
		 * 
		 * String documentName = modelName; if (model instanceof Selection) { documentName = ((Selection) model).getDocumentName();
		 * }
		 */
		return result;
	}

	private static ViewImpl generateEditView(Customer customer, Module module, Document document) 
	throws MetaDataException {
		ViewImpl result = new ViewImpl();
		result.setType(ViewType.edit);

		result.setTitle(document.getSingularAlias());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		result.putAction(action);

		AbstractRepository repository = AbstractRepository.get();

		// Add any actions that have privileges
		for (String actionName : ((DocumentImpl) document).getDefinedActionNames()) {
			action = new ActionImpl();
			try {
				repository.getAction(customer, document, actionName);
			}
			catch (Exception e) {
				try {
					repository.getUploadAction(customer, document, actionName);
					action.setImplicitName(ImplicitActionName.Upload);
				}
				catch (Exception e1) {
					try {
						repository.getBizExportAction(customer, document, actionName);
						action.setImplicitName(ImplicitActionName.BizExport);
					}
					catch (Exception e2) {
						repository.getBizImportAction(customer, document, actionName);
						action.setImplicitName(ImplicitActionName.BizImport);
					}
				}
			}

			action.setResourceName(actionName);
			action.setDisplayName(actionName);
			result.putAction(action);
		}

		Form form = new Form();
		FormColumn column = new FormColumn();
		column.setPercentageWidth(TWENTY);
		form.getColumns().add(column);
		form.getColumns().add(new FormColumn());
		form.getColumns().add(new FormColumn());
		
		List<MetaData> details = new ArrayList<>();

		processAttributes(customer, module, document, form, details);

		// make a tabbed view if more than 1 detail widget or there is 1 detail widget and more than 5 form fields
		int numberOfDetailWidgets = details.size();
		if ((numberOfDetailWidgets > 1) || 
				((numberOfDetailWidgets == 1) && (form.getRows().size() > 5))) {
			TabPane tabPane = new TabPane();
			Tab tab = null;
			if (! form.getRows().isEmpty()) {
				tab = new Tab();
				tab.setTitle("General");
				tab.getContained().add(form);
				tabPane.getTabs().add(tab);
			}
			
			for (MetaData detailWidget : details) {
				tab = new Tab();
				if (detailWidget instanceof TabularWidget) {
					TabularWidget tw = (TabularWidget) detailWidget;
					tab.setTitle(tw.getTitle());
					tw.setTitle(null);
				}
				else if (detailWidget instanceof ListMembership) {
					ListMembership lm = (ListMembership) detailWidget;
					tab.setTitle(lm.getCandidatesHeading());
				}
				tab.getContained().add(detailWidget);
				tabPane.getTabs().add(tab);
			}
			result.getContained().add(tabPane);
		}
		else {
			if (! form.getRows().isEmpty()) {
				result.getContained().add(form);
			}

			for (MetaData detailWidget : details) {
				result.getContained().add(detailWidget);
			}
		}

		return result;
	}

	private static void processAttributes(Customer customer, 
											Module module, 
											Document document,
											Form form,
											List<MetaData> details)
	throws MetaDataException {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			processAttributes(customer, module, baseDocument, form, details);
		}

		for (Attribute attribute : document.getAttributes()) {
			String attributeName = attribute.getName();

			if (attribute instanceof Collection) {
				Collection collection = (Collection) attribute;
				Document detailDocument = module.getDocument(customer, collection.getDocumentName());

				List<String> propertyNames = new ArrayList<>();
				populatePropertyNames(customer, module, detailDocument, propertyNames);
				
				if (collection.getDomainType() == null) {
					details.add(generateDataGrid(collection.getType(), 
													customer,
													module,
													detailDocument, 
													attributeName, 
													propertyNames));
				}
				else {
					ListMembership membership = new ListMembership();
					membership.setBinding(attribute.getName());
					membership.setCandidatesHeading("Candidates");
					membership.setMembersHeading("Members");
					details.add(membership);
				}
			}
			else if (attribute instanceof Inverse) {
				Inverse inverse = (Inverse) attribute;
				Document detailDocument = module.getDocument(customer, inverse.getDocumentName());

				List<String> propertyNames = new ArrayList<>();
				populatePropertyNames(customer, module, detailDocument, propertyNames);
				
				details.add(generateDataGrid(CollectionType.composition,
												customer,
												module,
												detailDocument, 
												attributeName, 
												propertyNames));
			}
			else { // field or association
				FormItem item = new FormItem();
				DefaultWidget widget = new DefaultWidget();
				widget.setBinding(attributeName);
				item.setWidget(widget);
				FormRow row = new FormRow();
				row.getItems().add(item);
				form.getRows().add(row);
			}
		}
	}
	
	private static void populatePropertyNames(Customer customer,
												Module module,
												Document document,
												List<String> propertyNames)
	throws MetaDataException {
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

	private static TextField generateTextField(String fieldName) {
		TextField result = new TextField();

		result.setBinding(fieldName);

		return result;
	}

	private static DataGrid generateDataGrid(CollectionType collectionType,
												Customer customer,
												Module module,
												Document document,
												String dataGridBinding,
												List<String> propertyNames)
	throws MetaDataException {
		DataGrid result = new DataGrid();
		if (dataGridBinding != null) {
			result.setBinding(dataGridBinding);
			result.setTitle(document.getPluralAlias());
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

				// Field has a domain 
				if (attribute instanceof Field) {
					Field field = (Field) attribute;

					column.setBinding(propertyName);
					
					DomainType domainType = field.getDomainType();
					if (DomainType.dynamic.equals(domainType)) {
						// Cannot determine the variant domain values client side so
						// widget needs to be a disabled text field - need to zoom to edit
						TextField textField = generateTextField(propertyName);
						textField.setDisabledConditionName("true");
						WidgetReference ref = new WidgetReference();
						ref.setWidget(textField);
						column.setInputWidget(ref);
					}
				}
				else if (attribute instanceof Association) {
					column.setBinding(propertyName);
					
					if (DomainType.dynamic.equals(attribute.getDomainType())) {
						// Cannot determine the variant domain values client side so
						// widget needs to be a disabled text field - need to zoom to edit
						TextField textField = generateTextField(propertyName);
						textField.setDisabledConditionName("true");
						WidgetReference ref = new WidgetReference();
						ref.setWidget(textField);
						column.setInputWidget(ref);
					}
				}
				else {
					throw new IllegalStateException("Attribute is neither a field nor an association...");
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
	
	public static String generateEditViewXML(Customer customer,
												Document document,
												boolean customerOverridden,
												boolean uxuiOverridden)
	throws MetaDataException {
		ViewImpl view = generate(customer, document, ViewType.edit);
		
		ViewMetaData repositoryView = new ViewMetaData();
		repositoryView.setType(ViewType.edit);
		repositoryView.setTitle(view.getTitle());

		repositoryView.getContained().addAll(view.getContained());

		for (Action action : view.getActions()) {
			repositoryView.getActions().add(((ActionImpl) action).toRepositoryAction());
		}

		return XMLUtil.marshalView(repositoryView, customerOverridden, uxuiOverridden);
	}
	
	public static void main(String[] args) throws Exception {
		String customerName = null;
		String moduleName = null;
		String documentName = null;
		String customerOverridden = null;
		String uxuiOverridden = null;
		
		if (args.length == 5) {
			customerName = args[0];
			moduleName = args[1];
			documentName = args[2];
			customerOverridden = args[3];
			uxuiOverridden = args[4];
		}
		else {
			System.err.println("Usage: org.skyve.wildcat.generate.ViewGenerator customerName moduleName documentName customerOverridden (boolean) uxuiOverridden (boolean)");
			System.exit(1);
		}

		AbstractRepository.set(new LocalDesignRepository());
		AbstractRepository repository = AbstractRepository.get();
		Customer customer = repository.getCustomer(customerName);
		Module module = repository.getModule(customer, moduleName);
		Document document = repository.getDocument(customer, module, documentName);
		File file = new File("./generatedEdit.xml");
		UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
		try (PrintWriter out = new PrintWriter(file)) {
			out.println(generateEditViewXML(customer, 
												document, 
												Boolean.parseBoolean(customerOverridden), 
												Boolean.parseBoolean(uxuiOverridden)));
			out.flush();
		}
	}
}
