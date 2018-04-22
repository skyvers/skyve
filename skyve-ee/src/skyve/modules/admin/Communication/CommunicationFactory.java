package modules.admin.Communication;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.DataFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Communication;

public class CommunicationFactory extends DataFactory {
	// Ensure that system is switched off so that the communication can be deleted by the SAIL test.
	@SkyveFixture(types = FixtureType.sail)
	public Communication sail() {
		Communication result = new DataBuilder(getUser()).build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		result.setSystem(Boolean.FALSE);
		return result;
	}
}
