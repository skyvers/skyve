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
	Q_ESCAPING_FIXTURE("qEscapingFixture");

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
