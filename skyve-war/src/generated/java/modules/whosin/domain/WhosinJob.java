package modules.whosin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.module.ModuleJob;

/**
 * Compile-time references to the jobs declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum WhosinJob implements ModuleJob {
	/** The "jLoadDemoData" job. */
	J_LOAD_DEMO_DATA("jLoadDemoData");

	private final String name;

	private WhosinJob(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "whosin";
	}

	@Override
	public String jobName() {
		return name;
	}
}
