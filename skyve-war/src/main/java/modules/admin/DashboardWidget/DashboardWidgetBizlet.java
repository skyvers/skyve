package modules.admin.DashboardWidget;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

import modules.admin.ModulesUtil;
import modules.admin.Dashboard.DashboardUtil;
import modules.admin.domain.Audit;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.Group;
import modules.admin.domain.Job;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;

public class DashboardWidgetBizlet extends Bizlet<DashboardWidgetExtension> {
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		if (DashboardWidget.moduleEntityPropertyName.equals(attributeName)) {
			List<DomainValue> results = new ArrayList<>();
			results.add(DashboardUtil.documentDomainValue(Audit.DOCUMENT_NAME));
			results.add(DashboardUtil.documentDomainValue(User.DOCUMENT_NAME));
			results.add(DashboardUtil.documentDomainValue(Group.DOCUMENT_NAME));
			results.add(DashboardUtil.documentDomainValue(Job.DOCUMENT_NAME));
			results.add(DashboardUtil.documentDomainValue(UserRole.DOCUMENT_NAME));
			Collections.sort(results, new ModulesUtil.DomainValueSortByDescription());
			return results;
		}
		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, DashboardWidgetExtension bean)
			throws Exception {

		if (bean.getModuleEntity() != null) {
			if (DashboardWidget.categoryBindingPropertyName.equals(attributeName)) {
				List<DomainValue> results = new ArrayList<>();
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(Dashboard.MODULE_NAME);
				Document document = module.getDocument(customer, bean.getModuleEntity());
				for (Attribute a : document.getAllAttributes(customer)) {
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
				Collections.sort(results, new ModulesUtil.DomainValueSortByDescription());
				return results;
			}

			if (DashboardWidget.valueBindingPropertyName.equals(attributeName)) {
				List<DomainValue> results = new ArrayList<>();
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

}
