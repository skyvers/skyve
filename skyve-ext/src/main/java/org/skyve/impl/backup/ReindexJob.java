package org.skyve.impl.backup;

import org.skyve.job.CancellableJob;

/**
 * Composite job that reindexes both bean content and binary attachments by
 * sequentially executing {@link ReindexAttachmentsJob} then {@link ReindexBeansJob}.
 */
public class ReindexJob extends CancellableJob {
	@Override
	public void execute() throws Exception {
		execute(new ReindexAttachmentsJob());
		execute(new ReindexBeansJob());
		setPercentComplete(100);
	}
}
