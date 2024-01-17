package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.Util;

/**
 * This job visits all the metadata for each customer after app deployment.
 * This job is executed only if access control is on so that the metadata is loaded and ready.
 * 
 * @author mike
 */
public class ValidateMetaDataJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			Util.LOGGER.info("Validate metadata");
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			for (String customerName : repository.getAllCustomerNames()) {
				DomainGenerator.validate(repository, customerName);
			}
			Util.LOGGER.info("Successfully validated metadata");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst validating metadata", e);
		}
	}
}
