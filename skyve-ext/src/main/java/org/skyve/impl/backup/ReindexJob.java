package org.skyve.impl.backup;

import org.skyve.job.CancellableJob;

public class ReindexJob extends CancellableJob {
	@Override
	public void execute() throws Exception {
		execute(new ReindexAttachmentsJob());
		execute(new ReindexBeansJob());
		setPercentComplete(100);
	}
}
