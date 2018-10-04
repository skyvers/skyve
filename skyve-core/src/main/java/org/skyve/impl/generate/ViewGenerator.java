package org.skyve.impl.generate;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.WidgetReference;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
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
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;

public class ViewGenerator {
	private static final Integer THIRTY = new Integer(30);
	private static final Integer SIXTY = new Integer(60);
	private static final Integer FOUR = new Integer(4);
	private static final Integer TWELVE = new Integer(12);
	
	private ViewGenerator() {
		// do nothing
	}

	public static ViewImpl generate(Customer customer, Document document, String viewName) {
		ViewImpl result = null;

		Module module = customer.getModule(document.getOwningModuleName());

		if (ViewType.list.toString().equals(viewName)) {
			QueryDefinition defaultQuery = module.getDocumentDefaultQuery(customer, document.getName());
			result = generateListView(customer, document, defaultQuery, null);
		}
		else if (ViewType.edit.toString().equals(viewName)) {
			result = generateEditView(customer, module, document);
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
			finalDescription = document.getDescription();
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

	private static class Detail {
		String title;
		MetaData widget;
	}
	
	private static ViewImpl generateEditView(Customer customer, Module module, Document document) {
		ViewImpl result = new ViewImpl();
		result.setName(ViewType.edit.toString());

		result.setTitle(document.getSingularAlias());

		ActionImpl action = new ActionImpl();
		action.setImplicitName(ImplicitActionName.DEFAULTS);
		result.putAction(action);

		AbstractRepository repository = AbstractRepository.get();

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

		Form form = new Form();
		form.setBorder(Boolean.TRUE);
		form.setPercentageWidth(SIXTY);
		form.setResponsiveWidth(TWELVE);
		FormColumn column = new FormColumn();
		column.setPercentageWidth(THIRTY);
		column.setResponsiveWidth(FOUR);
		form.getColumns().add(column);
		form.getColumns().add(new FormColumn());
		
		List<Detail> details = new ArrayList<>();

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
				result.getContained().add(form);
			}

			for (Detail detail : details) {
				result.getContained().add(detail.widget);
			}
		}

		return result;
	}

	private static void processAttributes(Customer customer, 
											Module module, 
											Document document,
											Form form,
											List<Detail> details) {
		Extends inherits = document.getExtends();
		if (inherits != null) {
			Document baseDocument = module.getDocument(customer, inherits.getDocumentName());
			processAttributes(customer, module, baseDocument, form, details);
		}

		for (Attribute attribute : document.getAttributes()) {
			if (! attribute.isDeprecated()) {
				String attributeName = attribute.getName();
	
				if (attribute instanceof Collection) {
					Collection collection = (Collection) attribute;
					Document detailDocument = module.getDocument(customer, collection.getDocumentName());
	
					List<String> propertyNames = new ArrayList<>();
					populatePropertyNames(customer, module, detailDocument, propertyNames);
					
					if (collection.getDomainType() == null) {
						@SuppressWarnings("synthetic-access")
						Detail detail = new Detail();
						detail.title = attribute.getDisplayName();
						detail.widget = generateDataGrid(collection.getType(), 
															customer,
															module,
															detailDocument, 
															attributeName, 
															propertyNames);
						details.add(detail);
					}
					else {
						@SuppressWarnings("synthetic-access")
						Detail detail = new Detail();
						detail.title = attribute.getDisplayName();
						ListMembership membership = new ListMembership();
						membership.setBinding(attribute.getName());
						membership.setCandidatesHeading("Candidates");
						membership.setMembersHeading("Members");
						detail.widget = membership;
						details.add(detail);
					}
				}
				else if (attribute instanceof Inverse) {
					Inverse inverse = (Inverse) attribute;
					Document detailDocument = module.getDocument(customer, inverse.getDocumentName());
	
					List<String> propertyNames = new ArrayList<>();
					populatePropertyNames(customer, module, detailDocument, propertyNames);
					
					@SuppressWarnings("synthetic-access")
					Detail detail = new Detail();
					detail.title = attribute.getDisplayName();
					detail.widget = generateDataGrid(CollectionType.composition,
														customer,
														module,
														detailDocument, 
														attributeName, 
														propertyNames);
					details.add(detail);
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

				DomainType domainType = attribute.getDomainType();
				if (DomainType.dynamic.equals(domainType)) {
					column.setBinding(Binder.createCompoundBinding(propertyName, Bean.BIZ_KEY));
					column.setEditable(Boolean.FALSE);
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
	
	public static String generateEditViewXML(Customer customer,
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
			moduleName = args[2];
			documentName = args[3];
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

		AbstractRepository.set(new LocalDesignRepository());
		AbstractRepository repository = AbstractRepository.get();
		Customer customer = repository.getCustomer(customerName);

		// If the module and/or document was not specified, we will just generate all edit views.
		if (StringUtils.isBlank(moduleName) || StringUtils.isBlank(documentName)) {
			for (Module module : customer.getModules()) {
				for (Map.Entry<String, Module.DocumentRef> entry : module.getDocumentRefs().entrySet()) {
					Module.DocumentRef documentRef = entry.getValue();
					if (documentRef.getOwningModuleName().equals(module.getName())) {
						Document document = module.getDocument(customer, entry.getKey());
						try {
							writeEditView(srcPath, module, document, customer, customerOverridden, uxui);
						} catch (Exception e) {
							UtilImpl.LOGGER.warning(String.format("Failed to generate edit view for %s.%s, %s.",
									module.getName(), document.getName(), e.getMessage()));
						}
					}
				}
			}
		} else {
			Module module = repository.getModule(customer, moduleName);
			Document document = repository.getDocument(customer, module, documentName);
			writeEditView(srcPath, module, document, customer, customerOverridden, uxui);
		}
	}

	private static void writeEditView(String srcPath, Module module, Document document, Customer customer,
							   boolean customerOverridden,
							   String uxui) throws IOException {
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
	}
}
