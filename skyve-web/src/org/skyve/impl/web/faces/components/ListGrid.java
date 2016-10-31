package org.skyve.impl.web.faces.components;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.faces.application.Application;
import javax.faces.component.FacesComponent;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlPanelGroup;
import javax.faces.context.FacesContext;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.component.button.Button;
import org.primefaces.component.column.Column;
import org.primefaces.component.datalist.DataList;
import org.primefaces.component.datatable.DataTable;
import org.primefaces.component.outputpanel.OutputPanel;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.UserAgent.UserAgentType;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.pipeline.component.ComponentRenderer;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;
import org.skyve.web.WebAction;

@FacesComponent(ListGrid.COMPONENT_TYPE)
public class ListGrid extends HtmlPanelGroup {
	@SuppressWarnings("hiding")
	public static final String COMPONENT_TYPE = "org.skyve.impl.web.faces.components.ListGrid";

	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		if (getChildCount() == 0) {
			Map<String, Object> attributes = getAttributes();
			final String moduleName = (String) attributes.get("module");
			final String queryName = (String) attributes.get("query");
			final String managedBeanName = (String) attributes.get("managedBean");
			final UserAgentType type = UserAgentType.valueOf((String) attributes.get("type"));
			Object canCreateAttribute = attributes.get("canCreate");
			final boolean canCreate = (canCreateAttribute == null) || "true".equals(canCreateAttribute);
			final boolean paginator = "true".equals(attributes.get("paginator"));
			
			new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					User user = CORE.getUser();
					Customer customer = user.getCustomer();
					Module module = customer.getModule(moduleName);
					DocumentQueryDefinition query = module.getDocumentQuery(queryName);
					if (query == null) {
						query = module.getDocumentDefaultQuery(customer, queryName);
					}
					
					FacesContext fc = FacesContext.getCurrentInstance();
					Application a = fc.getApplication();
					ExpressionFactory ef = a.getExpressionFactory();
				    ELContext elc = fc.getELContext();
//					UIForm form = (UIForm) a.createComponent(UIForm.COMPONENT_TYPE);
//					form.setPrependId(false);
//					form.getChildren().add(build(customer, query, a, ef, elc, managedBeanName, true, paginator, type));
//					ListGrid.this.getChildren().add(form);
				    ListGrid.this.getChildren().add(build(customer, query, a, ef, elc, managedBeanName, canCreate, paginator, type));
				    
					return null;
				}
			}.execute();
		}

		if ((UtilImpl.FACES_TRACE) && (! context.isPostback())) Util.LOGGER.info(new ComponentRenderer(this).toString());

		super.encodeBegin(context);
	}
	
	/*
		<h:form id="listForm" prependId="false">
			<p:dataTable id="list"
							var="row"
							value="#{skyve.getBeans(skyve.bizModuleParameter, skyve.queryNameParameter)}">
				<f:facet name="header">
					<p:outputPanel>
						Contacts
						<p:button href="./?a=#{WebAction.e.toString()}&amp;m=#{skyve.bizModuleParameter}&amp;d=#{skyve.bizDocumentParameter}" value="New" />
					</p:outputPanel>
				</f:facet>
				<p:column headerText="Name">
					<h:outputText value="#{row['bizKey']}" />
				</p:column>
				<p:column headerText="Actions" style="width:75px">
					<h:outputLink value="./">
						<h:outputText value="Edit" />
						<f:param name="a" value="#{WebAction.e.toString()}" />
						<f:param name="f" value="t" />
						<f:param name="m" value="#{row['bizModule']}" />
						<f:param name="d" value="#{row['bizDocument']}" />
						<f:param name="i" value="#{row['bizId']}" />
					</h:outputLink>
				</p:column>
			</p:dataTable>
		</h:form>
		
		OR
		
		<h:form>
			<p:dataList id="list" 
							var="row"
							paginator="true" 
							rows="10" 
							value="#{skyve.getBeans(skyve.bizModuleParameter, skyve.queryNameParameter)}">
				<f:facet name="header">
					<p:button href="./?a=#{WebAction.e.toString()}&amp;m=#{skyve.bizModuleParameter}&amp;d=#{skyve.bizDocumentParameter}" value="New" />
				</f:facet>
				<f:attribute name="paginatorText" value="More..." />
				<f:attribute name="filter" value="true" />
				<h:outputLink value="/?a=e&amp;m=#{row['bizModule']}&amp;d=#{row['bizDocument']}&amp;i=#{row['bizId']}">
					<h2><h:outputText value="#{row['bizKey']}" /></h2>
					<p><h:outputText value="#{row['bizKey']}" /></p>
				</h:outputLink>
			</p:dataList>
		</h:form>
	*/
	UIComponent build(Customer customer,
						DocumentQueryDefinition query,
						Application a,
						ExpressionFactory ef,
						ELContext elc,
						String managedBeanName,
						boolean canCreate,
						boolean paginator,
						UserAgentType type) 
	throws MetaDataException {
		String moduleName = query.getOwningModule().getName();
		String documentName = query.getDocumentName();

		UIComponent result = null;
		if (UserAgentType.phone.equals(type)) {
			DataList list = (DataList) a.createComponent(DataList.COMPONENT_TYPE);
			list.setVar("row");
			list.setPaginator(paginator);
			if (paginator) {
				list.setRows(10);
			}
			result = list;
		}
		else {
			DataTable table = (DataTable) a.createComponent(DataTable.COMPONENT_TYPE);
	        table.setVar("row");
	        table.setPaginator(paginator);
	        if (paginator) {
		        table.setRowsPerPageTemplate("25,50,75,100");
		        table.setRows(100);
	        }
	        table.setLazy(true);
	        table.setEmptyMessage("No Items to show");
	        table.setStickyHeader(true);
	        
        	String id = getId() + "_grid";
        	table.setId(id);
        	table.setWidgetVar(id);
	        table.setSelectionMode("single");
	        table.setValueExpression("rowKey", ef.createValueExpression(elc, "#{row['bizId']}", String.class));
	        
	        AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
	        StringBuilder start = new StringBuilder(64);
	        start.append("var s=PF('").append(id).append("').selection[0];window.location='");
			start.append("?a=").append(WebAction.e.toString());
			start.append("&m=").append(moduleName).append("&d=").append(documentName).append("&i='+s;return false;");
			ajax.setOnstart(start.toString());
	        table.addClientBehavior("rowSelect", ajax);
			
	        result = table;
		}
		
        StringBuilder value = new StringBuilder(128);

// TEMPORARY STUFF BELOW - uncomment one day when list models are introduced.
        value.append("#{").append(managedBeanName).append(".getBeans('").append(moduleName).append("', '");
        value.append(query.getName()).append("', null)}");
        result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), List.class));
/* Temporarily commented out but should be reinstated when we use the list model.
        if (UserAgentType.phone.equals(type)) {
            value.append("#{").append(managedBeanName).append(".getBeans('").append(moduleName).append("', '");
            value.append(query.getName()).append("', null)}");
            result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), List.class));
        }
        else {
	        value.append("#{").append(managedBeanName).append(".getModel('").append(moduleName).append("', '");
	        value.append(query.getName()).append("')}");
	        result.setValueExpression("value", ef.createValueExpression(elc, value.toString(), QueryDataModel.class));
        }
*/
        addHeader(result, a, query, moduleName, documentName, canCreate, UserAgentType.phone.equals(type));
        List<UIComponent> children = result.getChildren();
        addBoundColumns(customer, moduleName, documentName, query, children, a, ef, elc, type);
        if (! UserAgentType.phone.equals(type)) {
        	addActionColumn(moduleName, documentName, canCreate, children, a, ef, elc);
        }
        
		return result;
	}
	
	private static void addHeader(UIComponent componentToAddTo,
									Application a,
									QueryDefinition query,
									String moduleName,
									String documentName,
									boolean canCreate,
									boolean mobile) {
		if (mobile) {
			if (canCreate) {
				Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
	        	button.setValue("New");
	        	button.setTitle("New record");
	        	StringBuilder value = new StringBuilder(128);
	        	value.append("./?a=").append(WebAction.e.toString()).append("&m=").append(moduleName);
	        	value.append("&d=").append(documentName);
	        	button.setHref(value.toString());

		        OutputPanel headingPanel = (OutputPanel) a.createComponent(OutputPanel.COMPONENT_TYPE);
		        headingPanel.getChildren().add(button);
		        componentToAddTo.getFacets().put("header", headingPanel);
			}
		}
		else {
			UIOutput heading = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
	        heading.setValue(query.getDescription());
    		componentToAddTo.getFacets().put("header", heading);
		}
	}
	
	private static void addBoundColumns(Customer customer,
											String moduleName,
											String documentName,
											DocumentQueryDefinition query,
											List<UIComponent> componentChildrenToAddTo,
											Application a,
											ExpressionFactory ef,
											ELContext elc,
											UserAgentType type)
	throws MetaDataException {
		StringBuilder value = new StringBuilder(128);
		Module module = null;
		Document document = null;
		
		if (! UserAgentType.phone.equals(type)) {
			module = customer.getModule(moduleName);
			document = module.getDocument(customer, documentName);
		}
		
		int columnPriority = 1;

		for (QueryColumn queryColumn : query.getColumns()) {
			if (queryColumn.isHidden() || (! queryColumn.isProjected())) {
				continue;
			}
			
			if (! UserAgentType.phone.equals(type)) {
				String displayName = queryColumn.getDisplayName();
				if (displayName == null) {
					String binding = queryColumn.getBinding();
					if (binding != null) {
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
						Document bindingDocument = target.getDocument();
						Attribute bindingAttribute = target.getAttribute();
						if (binding.endsWith(Bean.BIZ_KEY)) {
							if (bindingDocument != null) {
								displayName = bindingDocument.getSingularAlias();
							}
							else {
								displayName = DocumentImpl.getBizKeyAttribute().getDisplayName();
							}
						}
						else if (binding.endsWith(ChildBean.ORDINAL_KEY)) {
							displayName = DocumentImpl.getBizOrdinalAttribute().getDisplayName();
						}
						else if (bindingAttribute != null) {
							displayName = bindingAttribute.getDisplayName();
						}
					}
				}
				Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
				column.setHeaderText(displayName);
				column.setPriority(columnPriority);
				if (columnPriority < 6) {
					columnPriority++;
				}
				
/* TODO complete this
				column.setSortBy(queryColumn.getBinding());
				column.setFilterBy(queryColumn.getBinding());
*/
				value.append("#{row['{").append(queryColumn.getBinding()).append("}']}");

				UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
				outputText.setValueExpression("value", ef.createValueExpression(elc, value.toString(), Object.class));
				column.getChildren().add(outputText);
				componentChildrenToAddTo.add(column);

				value.setLength(0);
			}
			else {
				boolean first = (value.length() == 0);
				value.append(first ? "<h2>" : "<p>");
				value.append("#{row['{").append(queryColumn.getBinding()).append("}']}");
				value.append(first ? "</h2>" : "</p>");
			}
		}
		
		if (UserAgentType.phone.equals(type)) {
			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValueExpression("value", ef.createValueExpression(elc, value.toString(), Object.class));

			HtmlOutputLink link = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
			value.setLength(0);
			value.append("./?a=").append(WebAction.e.toString());
			value.append("&m=#{row['bizModule']}&d=#{row['bizDocument']}&i=#{row['bizId']}");
			link.setValueExpression("value", ef.createValueExpression(elc, value.toString(), String.class));
			link.getChildren().add(outputText);
			componentChildrenToAddTo.add(link);
		}
	}

	private static void addActionColumn(String moduleName,
											String documentName,
											boolean canCreate,
											List<UIComponent> componentChildrenToAddTo,
											Application a,
											ExpressionFactory ef,
											ELContext elc) {
		Column column = (Column) a.createComponent(Column.COMPONENT_TYPE);
		column.setPriority(1);
		column.setWidth("40");
		column.setStyle("text-align:center !important");
        if (canCreate) {
	    	Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
	    	button.setValue(null);
        	button.setTitle("New record");
	    	button.setIcon("fa fa-plus");
        	StringBuilder value = new StringBuilder(128);
        	value.append("./?a=").append(WebAction.e.toString()).append("&m=").append(moduleName);
        	value.append("&d=").append(documentName);
        	button.setHref(value.toString());

	        column.getFacets().put("header", button);
        }
        else {
    		column.setHeaderText("");
        }

//		if (UserAgentType.tablet.equals(type)) {
	    	Button button = (Button) a.createComponent(Button.COMPONENT_TYPE);
	    	button.setValue(null);
	    	button.setTitle("View Detail");
	    	button.setIcon("fa fa-chevron-right");
			StringBuilder value = new StringBuilder(128);
			value.append("./?a=").append(WebAction.e.toString());
			value.append("&m=#{row['bizModule']}&d=#{row['bizDocument']}&i=#{row['bizId']}");
			button.setValueExpression("href", ef.createValueExpression(elc, value.toString(), String.class));
			column.getChildren().add(button);
			componentChildrenToAddTo.add(column);
/*
		}
		else {
			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValue("Edit");
			HtmlOutputLink link = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
			StringBuilder value = new StringBuilder(128);
			value.append("./?a=").append(WebAction.e.toString());
			value.append("&m=#{row['bizModule']}&d=#{row['bizDocument']}&i=#{row['bizId']}");
			link.setValueExpression("value", ef.createValueExpression(elc, value.toString(), String.class));
			link.getChildren().add(outputText);
			column.getChildren().add(link);
			componentChildrenToAddTo.add(column);
		}
*/
	}

//TODO Add sorting of columns
//TODO Add filter to phone dataList
}
