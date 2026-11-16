package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.query.ModuleQuery;

/**
 * Compile-time references to the queries declared in the kitchensink module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum KitchensinkQuery implements ModuleQuery {
	/** The "qEscapingFixture" query. */
	Q_ESCAPING_FIXTURE("qEscapingFixture"),
	/** The "qModelLookupValues" query. */
	Q_MODEL_LOOKUP_VALUES("qModelLookupValues"),
	/** The "qModelLookupFixtures" query. */
	Q_MODEL_LOOKUP_FIXTURES("qModelLookupFixtures"),
	/** The "qListGridFixtureVue" query. */
	Q_LIST_GRID_FIXTURE_VUE("qListGridFixtureVue"),
	/** The "qListGridFixtureFaces" query. */
	Q_LIST_GRID_FIXTURE_FACES("qListGridFixtureFaces"),
	/** The "qListGridFixtureVueAttributes" query. */
	Q_LIST_GRID_FIXTURE_VUE_ATTRIBUTES("qListGridFixtureVueAttributes"),
	/** The "qListGridFixtureFacesAttributes" query. */
	Q_LIST_GRID_FIXTURE_FACES_ATTRIBUTES("qListGridFixtureFacesAttributes"),
	/** The "qConditionsFixtureListAttributes" query. */
	Q_CONDITIONS_FIXTURE_LIST_ATTRIBUTES("qConditionsFixtureListAttributes"),
	/** The "qConditionsFixtureNodes" query. */
	Q_CONDITIONS_FIXTURE_NODES("qConditionsFixtureNodes");

	private final String name;

	private KitchensinkQuery(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "kitchensink";
	}

	@Override
	public String queryName() {
		return name;
	}
}
