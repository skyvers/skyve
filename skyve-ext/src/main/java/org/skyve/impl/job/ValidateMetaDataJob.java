package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This job visits all the metadata for each customer after app deployment.
 * This job is executed only if access control is on so that the metadata is loaded and ready.
 * 
 * @author mike
 */
public class ValidateMetaDataJob implements Job {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValidateMetaDataJob.class);

	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			LOGGER.info("Validate metadata");
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			for (String customerName : repository.getAllCustomerNames()) {
				DomainGenerator.validate(repository, customerName);
			}
			LOGGER.info("Successfully validated metadata");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst validating metadata", e);
		}
	}
}
