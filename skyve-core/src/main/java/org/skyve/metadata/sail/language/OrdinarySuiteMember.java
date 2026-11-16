package org.skyve.metadata.sail.language;

import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * An ordinary member of a {@link Suite}: a {@code <member>} element. It runs after every set-up member has passed (it
 * requires them without naming them), and does not stop the suite's other members if it fails.
 *
 * <p>Unlike a set-up or restore member, an ordinary member accepts a run's test strategy override, and the suite's
 * default, unless it sets {@code lockTestStrategy="true"}; see {@link Suite#isTestStrategyLocked(SuiteMember)}.
 *
 * <p>Not thread-safe: a member is built, or unmarshalled, before a suite runs.
 *
 * @see Suite
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"lockTestStrategy"})
public class OrdinarySuiteMember extends SuiteMember {
	private Boolean lockTestStrategy;

	/**
	 * Returns the {@code lockTestStrategy} attribute as written.
	 *
	 * @return {@code true} if a run's test strategy override must not apply to this member; {@code null} if the
	 * 			attribute is absent, which means {@code false}
	 */
	public @Nullable Boolean getLockTestStrategy() {
		return lockTestStrategy;
	}

	/**
	 * Sets the {@code lockTestStrategy} attribute.
	 *
	 * @param lockTestStrategy	{@code true} to run this member with its own test strategy whatever the run asks for;
	 * 							{@code null} (absent) or {@code false} to accept a run's override
	 */
	@XmlAttribute(name = "lockTestStrategy")
	public void setLockTestStrategy(@Nullable Boolean lockTestStrategy) {
		this.lockTestStrategy = lockTestStrategy;
	}
}
