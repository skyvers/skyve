package modules.admin.ReportTemplate;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ReportTemplate;

@SkyveFactory(excludedUpdateAttributes = { ReportTemplate.runAsPropertyName })
public class ReportTemplateFactory {
	/**
	 * Set scheduled to false so that the update action does not require the runAs UserProxy,
	 * as validated by the bizlet.
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static ReportTemplate crudInstance() {
		ReportTemplate bean = new DataBuilder().fixture(FixtureType.crud).factoryBuild(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setScheduled(Boolean.TRUE);
		return bean;
	}
}
