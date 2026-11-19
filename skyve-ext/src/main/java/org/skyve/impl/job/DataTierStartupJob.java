package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UUIDv7;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nullable;

/**
 * Starts the content management system asynchronously, creates the configured bootstrap user, and invokes customer
 * data-tier-ready observers in the same super user persistence context.
 *
 * <p>Content startup can take a while, so this work does not block the app server deployment process.
 *
 * @author mike
 */
public class DataTierStartupJob implements Job {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DataTierStartupJob.class);

	@Override
	@SuppressWarnings("java:S1141") // nested try/catch OK here - its simple
	public void execute(@Nullable JobExecutionContext context)
	throws JobExecutionException {
		LOGGER.info("Starting the content manager");
		try (ContentManager cm = EXT.newContentManager()) {
			try {
				cm.startup();
			}
			catch (Exception e) {
				LOGGER.error("Could not start the content manager; bootstrap and data startup will not be attempted", e);
				return;
			}
			LOGGER.info("Completed startup of the content manager");

			try {
				bootstrapAndNotifyDataStartup();
			}
			catch (Exception e) {
				LOGGER.error("Could not complete bootstrap and data startup after content manager startup", e);
			}
		}
		catch (Exception e) {
			LOGGER.error("Could not close the content manager after startup", e);
		}
	}

	private static void bootstrapAndNotifyDataStartup() {
		AbstractPersistence persistence = null;
		try {
			persistence = (AbstractPersistence) CORE.getPersistence();
			persistence.begin();

			SuperUser user = new SuperUser();
			user.setCustomerName(UtilImpl.BOOTSTRAP_CUSTOMER);
			user.setContactName(UtilImpl.BOOTSTRAP_USER);
			user.setId(UUIDv7.create().toString());
			user.setName(UtilImpl.BOOTSTRAP_USER);
			user.setPasswordHash(EXT.hashPassword(UtilImpl.BOOTSTRAP_PASSWORD));
			persistence.setUser(user);

			if ((UtilImpl.ENVIRONMENT_IDENTIFIER != null) && (UtilImpl.BOOTSTRAP_CUSTOMER != null)) {
				EXT.bootstrap(persistence);
				persistence.commit(false);
				persistence.begin();
			}

			ProvidedRepository.notifyAllCustomersObservers(c -> ((CustomerImpl) c).notifyDataTierReady());
		}
		catch (Exception e) {
			if (persistence != null) {
				persistence.rollback();
			}
			throw new IllegalStateException("Cannot bootstrap.", e);
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}
}
