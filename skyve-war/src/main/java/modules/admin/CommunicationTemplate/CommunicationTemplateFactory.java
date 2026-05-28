package modules.admin.CommunicationTemplate;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.CommunicationTemplate;

/**
 * Creates Communication Template document instances with default values.
 */
@SkyveFactory(testDomain = false)
public class CommunicationTemplateFactory {

	/**
	 * Performs the crudInstance operation.
	 * @return the operation result
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static CommunicationTemplate crudInstance() {
		CommunicationTemplate bean = new DataBuilder().factoryBuild(CommunicationTemplate.MODULE_NAME,
				CommunicationTemplate.DOCUMENT_NAME);
		bean.setTemplate("<span>some text</span>{body}<span>some other text</span>");

		return bean;
	}

}
