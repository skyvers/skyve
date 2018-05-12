package modules.admin.Audit;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.DataFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

public class AuditFactory extends DataFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Audit crudInstance() {
		Audit audit = newAudit();

		Audit source = newAudit();
		audit.setSourceVersion(source);
		source.setSourceVersion(audit);

		return audit;
	}

	/**
	 * Completely override the super to avoid infinite recursion generating source version
	 */
	private static Audit newAudit() {
		Audit bean = new DataBuilder().build(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		bean.setOperation(Operation.insert);

		return bean;
	}

}
