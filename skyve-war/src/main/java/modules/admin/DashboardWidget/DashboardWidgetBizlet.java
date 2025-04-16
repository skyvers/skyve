package modules.admin.DashboardWidget;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;

public class DashboardWidgetBizlet extends Bizlet<DashboardWidgetExtension> {
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		if (DashboardWidget.dashboardModulePropertyName.equals(attributeName)) {
			List<DomainValue> results = new ArrayList<>();
			List<Module> modules = CORE.getCustomer()
					.getModules();
			for (Module module : modules) {
				results.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
			return results;
		}
		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, DashboardWidgetExtension bean)
			throws Exception {
		List<DomainValue> results = new ArrayList<>();
		if (bean.getDashboardModule() != null) {
			if (DashboardWidget.moduleEntityPropertyName.equals(attributeName)) {
				results.clear();
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(bean.getDashboardModule());
				Map<String, DocumentRef> documentsMap = module.getDocumentRefs();
				documentsMap.forEach((t, u) -> {
					Document document = module.getDocument(customer, t);
					if (document.isPersistable()) {
						results.add(new DomainValue(document.getName(), document.getLocalisedSingularAlias()));
					}
				});
				Collections.sort(results, new ModulesUtil.DomainValueSortByDescription());
				return results;
			}
		}

		if (bean.getModuleEntity() != null) {
			if (DashboardWidget.categoryBindingPropertyName.equals(attributeName)) {
				results.clear();
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(Dashboard.MODULE_NAME);
				Document document = module.getDocument(customer, bean.getModuleEntity());
				for (Attribute a : document.getAllAttributes(customer)) {
					if (a.isPersistent()) {
						if (AttributeType.association.equals(a.getAttributeType())
								|| AttributeType.enumeration.equals(a.getAttributeType())
								|| AttributeType.text.equals(a.getAttributeType())
								|| AttributeType.date.equals(a.getAttributeType())
								|| AttributeType.dateTime.equals(a.getAttributeType())
								|| AttributeType.integer.equals(a.getAttributeType())
								|| AttributeType.longInteger.equals(a.getAttributeType())
								|| AttributeType.decimal2.equals(a.getAttributeType())
								|| AttributeType.decimal5.equals(a.getAttributeType())
								|| AttributeType.decimal10.equals(a.getAttributeType())) {
							results.add(new DomainValue(a.getName(), a.getDisplayName()));
						}
					}
				}
				Collections.sort(results, new ModulesUtil.DomainValueSortByDescription());
				return results;
			}

			if (DashboardWidget.valueBindingPropertyName.equals(attributeName)) {
				results.clear();
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(Dashboard.MODULE_NAME);
				Document document = module.getDocument(customer, bean.getModuleEntity());
				for (Attribute a : document.getAllAttributes(customer)) {
					if (AttributeType.date.equals(a.getAttributeType())
							|| AttributeType.dateTime.equals(a.getAttributeType())
							|| AttributeType.integer.equals(a.getAttributeType())
							|| AttributeType.longInteger.equals(a.getAttributeType())
							|| AttributeType.decimal2.equals(a.getAttributeType())
							|| AttributeType.decimal5.equals(a.getAttributeType())
							|| AttributeType.decimal10.equals(a.getAttributeType())) {
						results.add(new DomainValue(a.getName(), a.getDisplayName()));
					}
				}
				results.add(new DomainValue(Bean.DOCUMENT_ID, "Item"));
				Collections.sort(results, new ModulesUtil.DomainValueSortByDescription());
				return results;
			}
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public void preRerender(String source, DashboardWidgetExtension bean, WebContext webContext) throws Exception {
		if (DashboardWidget.aggregateFunctionPropertyName.equals(source)
				|| DashboardWidget.valueBindingPropertyName.equals(source)) {
			// Check that aggregate function is not null while the valueBinding is a date/dateTime attribute. If so send a growl
			// message
			Customer customer = CORE.getCustomer();
			Module module = customer.getModule(bean.getDashboardModule());
			Document document = module.getDocument(customer, bean.getModuleEntity());
			Attribute attribute = document.getAttribute(bean.getValueBinding());
			if (attribute != null) {
				AttributeType type = attribute.getAttributeType();
				boolean valueBindingNotNumber = AttributeType.date.equals(type)
						|| AttributeType.dateTime.equals(type);
				boolean aggregateFunctionNull = bean.getAggregateFunction() == null;
	
				if (valueBindingNotNumber && aggregateFunctionNull) {
					webContext.growl(MessageSeverity.info,
							"Please ensure that the valueBinding is not of type Date/DateTime or is not \"Item\" when the aggregate function is null");
				}
			}
		}
		super.preRerender(source, bean, webContext);
	}

}
