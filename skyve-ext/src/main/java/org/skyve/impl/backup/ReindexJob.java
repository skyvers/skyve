package org.skyve.impl.backup;

import org.skyve.job.CancellableJob;

public class ReindexJob extends CancellableJob {
	private static final long serialVersionUID = -729525495498169081L;

	@Override
	public void execute() throws Exception {
		execute(new ReindexAttachmentsJob());
		execute(new ReindexBeansJob());
		setPercentComplete(100);
	}
}
