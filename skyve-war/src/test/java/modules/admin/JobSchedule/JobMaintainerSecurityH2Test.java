package modules.admin.JobSchedule;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * Verifies the {@code JobMaintainer} access required to select a user for
 * {@code JobSchedule.runAs}.
 * <p>
 * An {@code AppUser} can read only their own {@code UserProxy}. A job
 * maintainer must be able to resolve another user in the same customer without
 * also requiring the broader {@code BasicUser} role.
 */
class JobMaintainerSecurityH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void jobMaintainerCanResolveAnotherUserProxyInCustomerForRunAs() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		long suffix = System.nanoTime();
		UserExtension maintainer = user("job.maintainer." + suffix, "job-maintainer-" + suffix);
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.JobMaintainer");
		maintainer.addRolesElement(role);
		UserRole appUserRole = UserRole.newInstance();
		appUserRole.setRoleName("admin.AppUser");
		maintainer.addRolesElement(appUserRole);
		maintainer = persistence.save(maintainer);
		UserExtension runAsUser = user("job.run.as." + suffix, "job-run-as-" + suffix);
		UserRole runAsRole = UserRole.newInstance();
		runAsRole.setRoleName("admin.AppUser");
		runAsUser.addRolesElement(runAsRole);
		runAsUser = persistence.save(runAsUser);

		org.skyve.metadata.user.User metadataUser = maintainer.toMetaDataUser();
		assertNotNull(metadataUser);
		assertTrue(metadataUser.isInRole("admin", "JobMaintainer"));
		assertTrue(metadataUser.isInRole("admin", "AppUser"));
		assertFalse(metadataUser.isInRole("admin", "BasicUser"));
		persistence.setUser(metadataUser);

		Customer customer = metadataUser.getCustomer();
		Document userProxyDocument = customer.getModule(UserProxy.MODULE_NAME).getDocument(customer,
				UserProxy.DOCUMENT_NAME);
		Bean resolved = WebUtil.findReferencedBean(userProxyDocument, runAsUser.getBizId(), persistence, null,
				mockWebContext());

		assertEquals(runAsUser.getBizId(), resolved.getBizId());
	}

	private static UserExtension user(String userName, String bizId) {
		UserExtension result = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		result.setBizId(bizId);
		result.setUserName(userName);
		result.getGroups().clear();
		result.getRoles().clear();
		return result;
	}
}
