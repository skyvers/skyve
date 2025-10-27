package modules.admin.ReportDesign;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.metadata.module.menu.AbstractDocumentMenuItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.ReportDesign;
import modules.admin.domain.ReportDesign.DefinitionSource;
import modules.admin.domain.ReportDesign.Orientation;

public class ReportDesignBizlet extends Bizlet<ReportDesign> {
	@Inject
	private transient ReportDesignService reportDesignService;

	@Override
	public ReportDesign newInstance(ReportDesign bean) throws Exception {
		ReportDesign rd = super.newInstance(reportDesignService.beanDesignFromSpecification(bean, new DesignSpecification()));

		// populate the output directory from the JSON if provided
		if (Util.getModuleDirectory() != null) {
			rd.setRepositoryPath(Util.getModuleDirectory());
		}

		return rd;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();
		Persistence pers = CORE.getPersistence();

		Customer customer = pers.getUser().getCustomer();
		if (ReportDesign.moduleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ReportDesign bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (ReportDesign.documentNamePropertyName.equals(attributeName) && bean.getModuleName() != null) {
			Module module = customer.getModule(bean.getModuleName());
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				result.add(new DomainValue(document.getName(), document.getLocalisedDescription()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (ReportDesign.queryNamePropertyName.equals(attributeName) && bean.getModuleName() != null) {
			Module module = customer.getModule(bean.getModuleName());

			// Currently only document queries are supported.
			final List<QueryDefinition> documentQueries = module.getMetadataQueries().stream()
					.filter(q -> q instanceof MetaDataQueryDefinition)
					.map(q -> (MetaDataQueryDefinition) q)
					.collect(Collectors.toList());
			for (QueryDefinition queryDefinition : documentQueries) {
				result.add(new DomainValue(queryDefinition.getName(), queryDefinition.getLocalisedDescription()));
			}
			result.sort(Comparator.comparing(DomainValue::getLocalisedDescription));
		}

		if (ReportDesign.menuItemPropertyName.equals(attributeName) && bean.getModuleName() != null) {
			Module module = customer.getModule(bean.getModuleName());
			flattenToDocumentMenuItems(module.getMenu()).forEach(m -> result.add(new DomainValue(m.getName(), m.getName())));
		}

		return result;
	}

	private List<AbstractDocumentMenuItem> flattenToDocumentMenuItems(Menu menu) {
		final List<AbstractDocumentMenuItem> menuItems = new ArrayList<>();
		for (MenuItem menuItem : menu.getItems()) {
			if (menuItem instanceof AbstractDocumentMenuItem) {
				menuItems.add((AbstractDocumentMenuItem) menuItem);
			}

			if (menuItem instanceof Menu) {
				menuItems.addAll(flattenToDocumentMenuItems((Menu) menuItem));
			}
		}

		return menuItems;
	}

	@Override
	public void preRerender(String source, ReportDesign bean, WebContext webContext) throws Exception {

		final Customer customer = CORE.getPersistence().getUser().getCustomer();

		if (ReportDesign.documentNamePropertyName.equals(source) && bean.getName() == null) {
			// set a default name
			bean.setName("rpt" + bean.getDocumentName());
		}
		if (ReportDesign.defaultBorderPropertyName.equals(source)) {
			bean.setDefaultBorderTop(bean.getDefaultBorder());
			bean.setDefaultBorderLeft(bean.getDefaultBorder());
			bean.setDefaultBorderBottom(bean.getDefaultBorder());
			bean.setDefaultBorderRight(bean.getDefaultBorder());
		}
		if (ReportDesign.dynamicFlowPropertyName.equals(source) && Boolean.TRUE.equals(bean.getDynamicFlow())) {
			bean.setRenderLabelAsTextFields(Boolean.TRUE);
		}
		if (ReportDesign.queryNamePropertyName.equals(source) && StringUtils.isNotBlank(bean.getQueryName())) {
			bean.setDefinitionSource(DefinitionSource.query);

			final Module module = customer.getModule(bean.getModuleName());
			final MetaDataQueryDefinition query = module.getMetaDataQuery(bean.getQueryName());

			if (query != null) {
				bean.setDocumentName(query.getDocumentName());
			} else {
				throw new IllegalArgumentException("Selected query does not exist.");
			}
		}
		if (ReportDesign.menuItemPropertyName.equals(source) && StringUtils.isNotBlank(bean.getMenuItem())) {
			final Module module = customer.getModule(bean.getModuleName());
			final String menuItemName = bean.getMenuItem();
			final AbstractDocumentMenuItem menuItem = flattenToDocumentMenuItems(module.getMenu()).stream()
					.filter(m -> menuItemName.equals(m.getName()))
					.findFirst()
					.orElse(null);

			if (menuItem != null) {
				bean.setDocumentName(menuItem.getDocumentName());
				bean.setQueryName(null);

				if (menuItem instanceof EditItem) {
					bean.setDefinitionSource(DefinitionSource.view);
				} else if (menuItem instanceof ListItem) {
					bean.setDefinitionSource(DefinitionSource.list);
				} else {
					throw new IllegalArgumentException(String.format("Menu item %s is not supported.", menuItem.getName()));
				}
			} else {
				throw new IllegalArgumentException("Selected menu item does not exist.");
			}
		}

		resetDesign(bean);

		super.preRerender(source, bean, webContext);
	}

	@SuppressWarnings("boxing")
	private static void resetDesign(ReportDesign design) throws Exception {
		if (Orientation.portrait.equals(design.getOrientation())) {
			design.setWidth(595);
			design.setColumnWidth(595 - design.getLeftMargin() - design.getRightMargin());
			design.setHeight(842);
		} else {
			design.setWidth(842);
			design.setColumnWidth(842 - design.getLeftMargin() - design.getRightMargin());
			design.setHeight(595);
		}
	}
}
