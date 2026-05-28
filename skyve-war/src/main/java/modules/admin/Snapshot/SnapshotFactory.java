package modules.admin.Snapshot;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Snapshot;

/**
 * Provides fixture construction helpers for Snapshot document tests.
 */
public class SnapshotFactory {
	/**
	 * Creates a CRUD-oriented snapshot fixture with a unique query name.
	 *
	 * @return a populated snapshot fixture instance
	 */
	@SkyveFixture(types = FixtureType.crud)
	@SuppressWarnings("static-method")
	public Snapshot crudInstance() {
		Snapshot bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.domain(false)
				.factoryBuild(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);

		bean.setQueryName("snapshot" + System.nanoTime());

		return bean;
	}
}
