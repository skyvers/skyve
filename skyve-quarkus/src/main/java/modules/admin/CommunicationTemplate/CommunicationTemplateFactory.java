package modules.admin.CommunicationTemplate;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.CommunicationTemplate;

@SkyveFactory(testDomain = false)
public class CommunicationTemplateFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static CommunicationTemplate crudInstance() {
		CommunicationTemplate bean = new DataBuilder().build(CommunicationTemplate.MODULE_NAME,
				CommunicationTemplate.DOCUMENT_NAME);
		bean.setTemplate("<span>some text</span>{body}<span>some other text</span>");

		return bean;
	}

}
