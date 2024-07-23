package modules.admin.ReportDataset;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.BeanValidator;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;

public class ReportDatasetBizlet extends Bizlet<ReportDatasetExtension> {

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ReportDataset.datasetTypePropertyName.equals(attributeName)) {
			// SQL datasets are not available in multi-tenant applications
			List<DomainValue> domainValues = new ArrayList<>();
			for (DatasetType type : DatasetType.values()) {
				if (type == DatasetType.SQL && UtilImpl.CUSTOMER == null) {
					// don't show the SQL dataset type for multitenant applications
					continue;
				}
				domainValues.add(type.toDomainValue());
			}
			return domainValues;
		}

		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public ReportDatasetExtension preExecute(ImplicitActionName actionName, ReportDatasetExtension bean, Bean parentBean,
			WebContext webContext) throws Exception {

		if (ImplicitActionName.ZoomOut.equals(actionName)) {
			// validate the report parameters on the parent when zooming out of a dataset
			validateReportParameters(bean);
		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, ReportDatasetExtension bean, WebContext webContext) throws Exception {

		if (ReportDataset.queryPropertyName.equals(source)) {
			bean.addMissingParameters();
		}

		super.preRerender(source, bean, webContext);
	}

	/**
	 * Validates all the report parameters against the metadata. Used because the ReportDataset edit
	 * view won't validate the parameters as they belong to the parent ReportTemplate not the dataset
	 * being edited.
	 */
	private static void validateReportParameters(ReportDatasetExtension bean) {
		if (bean.getParent() != null) {
			List<ReportParameterExtension> parameters = bean.getParent().getParameters();
			if (parameters.size() > 0) {
				ValidationException e = new ValidationException();
				for (int i = 0; i < parameters.size(); i++) {
					ReportParameterExtension parameter = parameters.get(i);
					try {
						BeanValidator.validateBeanAgainstDocument(parameter);
					} catch (ValidationException ve) {
						for (Message m : ve.getMessages()) {
							// remap the bindings to a compound to the ReportDataset parameter grid
							List<String> newBindings = new ArrayList<>();
							for (Iterator<String> iterator = m.getBindings().iterator(); iterator.hasNext();) {
								String binding = iterator.next();
								newBindings.add(String.format("%s.%s", Binder.createCompoundBinding(ChildBean.PARENT_NAME,
											Binder.createIndexedBinding(ReportTemplate.parametersPropertyName, i)), binding));
							}
							e.getMessages().add(new Message(newBindings.toArray(new String[0]), m.getText()));
						}
					}
				}
				if (e.getMessages().size() > 0) {
					throw e;
				}
			}
		}
	}
}
