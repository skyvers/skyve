package modules.whosin.Staff;

import java.util.Random;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Contact.ContactExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.Contact.ContactType;
import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;
import modules.whosin.domain.WhosinDocument;

import jakarta.annotation.Nonnull;

/**
 * Generates seeded {@link Staff} fixtures for Who's In demonstration data.
 */
public class StaffFactory {
	@SuppressWarnings("java:S2245") // It's ok that this is not cryptographically strong as it's only used for generating test data
	private static final Random random = new Random();

	/**
	 * Creates a seeded staff instance with a person contact and either an existing or new office.
	 *
	 * @return a seeded staff instance suitable for fixture loading
	 */
	@SkyveFixture(types = FixtureType.seed)
	public static @Nonnull Staff seedInstance() {

		Staff bean = new DataBuilder().factoryBuild(WhosinDocument.STAFF);

		// create a new person type contact
		ContactExtension contact = new DataBuilder().fixture(FixtureType.seed).build(AdminDocument.CONTACT);
		contact.setContactType(ContactType.person);
		bean.setContact(contact);

		// throw the dice to see whether to create a new office, or re-use an existing one
		int dice = random.nextInt(50);
		if (dice < 49) {
			// re-use a random existing office
			DocumentQuery qOffice = CORE.getPersistence().newDocumentQuery(WhosinDocument.OFFICE);

			int officeCount = qOffice.beanResults().size();
			if (officeCount > 0) {
				int randomOfficeIndex = random.nextInt(officeCount);
				qOffice.setFirstResult(randomOfficeIndex);

				Office office = qOffice.beanResult();
				if (office != null) {
					bean.setBaseOffice(office);
				}
			}

		}
		if (bean.getBaseOffice() == null) {
			// create a new office
			Office office = new DataBuilder().fixture(FixtureType.seed).build(WhosinDocument.OFFICE);
			bean.setBaseOffice(office);
		}

		return bean;
	}
}
