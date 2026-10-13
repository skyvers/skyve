package modules.admin.ReportTemplate;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ReportTemplate;

/**
 * Fixture factory for {@code admin.ReportTemplate} test instances.
 *
 * <p>A scheduled report template needs a runAs user to pass the bizlet's validation on save,
 * which generated test data does not have. The generated update test is therefore kept from
 * changing either {@code runAs} or {@code scheduled}: switching {@code scheduled} on would
 * make the missing runAs user an error.
 */
@SkyveFactory(excludedUpdateAttributes = { ReportTemplate.runAsPropertyName, ReportTemplate.scheduledPropertyName })
public class ReportTemplateFactory {
	/**
	 * Builds an unscheduled report template, so that saving it does not require the runAs UserProxy,
	 * as validated by the bizlet.
	 *
	 * <p>The return type has to be the extension class for the data builder to use this method.
	 *
	 * @return an unscheduled report template
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static ReportTemplateExtension crudInstance() {
		ReportTemplateExtension bean = new DataBuilder().fixture(FixtureType.crud).factoryBuild(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		bean.setScheduled(Boolean.FALSE);
		return bean;
	}
}
