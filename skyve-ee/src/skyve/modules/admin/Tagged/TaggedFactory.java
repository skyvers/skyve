package modules.admin.Tagged;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.DataFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Tagged;

public class TaggedFactory extends DataFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Tagged crudInstance() throws Exception {
		return new DataBuilder().depth(1).build(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
	}

}
