package modules.admin.domain;

import modules.admin.util.SnapshotFactory;
import util.AbstractDomainTest;
import util.AbstractH2Test;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractH2Test} to create your own tests for this document.
 */
public class SnapshotTest extends AbstractDomainTest<Snapshot> {

	private SnapshotFactory factory;

	@Override
	public void setUp() throws Exception {
		factory = new SnapshotFactory();
	}

	@Override
	protected Snapshot getBean() throws Exception {
		return factory.getInstance();
	}
}