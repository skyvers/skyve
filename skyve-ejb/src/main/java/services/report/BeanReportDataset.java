package services.report;

import java.util.List;

import org.apache.commons.beanutils.DynaBean;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;

public interface BeanReportDataset {

	/**
	 * Returns a list of beans to be added into the report context when specified as
	 * part of a {@link ReportTemplate}. A {@link ReportDataset} will be required
	 * to specify the binding.
	 * 
	 * @param list List of parameters as specified in the {@link ReportTemplate}. Will be an empty collection when there are no
	 *        parameters.
	 * @return List of beans
	 */
	public List<DynaBean> getResults(final List<ReportParameterExtension> list);

}
