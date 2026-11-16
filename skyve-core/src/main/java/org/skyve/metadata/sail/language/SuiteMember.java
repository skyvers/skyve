package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlList;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL file that a {@link Suite} runs: a {@code <workflow>} file, or an {@code <automation>} file (a workflow of one).
 * This type is a suite's {@code <setUp>} and {@code <restore>}; an ordinary {@code <member>} is an
 * {@link OrdinarySuiteMember}.
 *
 * <p>{@code file} is relative to the suite file's folder (a classpath resource or a file). It also names the member:
 * the {@code requires} of other members refer to it by this value, exactly as written.
 *
 * <p>{@code requires} names the members (by their {@code file}) that must pass before this one runs. A member whose
 * requirement failed or was skipped is skipped. Set-up and restore members are always locked to their own test
 * strategy: a run's test strategy override does not apply to them.
 *
 * <p>Not thread-safe: a member is built, or unmarshalled, before a suite runs.
 *
 * @see Suite
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"file", "requiresAttribute"})
@XmlAccessorType(XmlAccessType.PROPERTY) // binds the package private requires attribute accessors
public class SuiteMember {
	private String file;
	private List<String> requires = new ArrayList<>();

	/**
	 * Returns the workflow or automation file, relative to the suite file's folder.
	 *
	 * @return the file; never {@code null} once set or unmarshalled
	 */
	public @Nonnull String getFile() {
		return file;
	}

	/**
	 * Sets the workflow or automation file, relative to the suite file's folder.
	 *
	 * @param file	the file; a blank value is stored as {@code null}, which {@link Suite#validate(String)} rejects
	 */
	@XmlAttribute(name = "file", required = true)
	public void setFile(@Nonnull String file) {
		this.file = Util.processStringValue(file);
	}

	/**
	 * Returns the files of the members that must pass before this member runs (the space-separated {@code requires}
	 * attribute), in the order written. An ordinary member also requires every set-up member without naming them; see
	 * {@link Suite#requirementsOf(SuiteMember)}.
	 *
	 * @return the required members' files (a live, possibly empty list)
	 */
	@XmlTransient
	public @Nonnull List<String> getRequires() {
		return requires;
	}

	/**
	 * Returns the {@code requires} attribute for JAXB: {@code null} when there are no requirements, so that
	 * marshalling omits the attribute instead of writing {@code requires=""}.
	 *
	 * @return the live list, or {@code null} if it is empty
	 */
	@XmlAttribute(name = "requires")
	@XmlList
	@Nullable List<String> getRequiresAttribute() {
		return requires.isEmpty() ? null : requires;
	}

	/**
	 * Sets the {@code requires} attribute for JAXB, replacing the requirements.
	 *
	 * @implNote JAXB sets a new, empty list and then adds the values to it, so the list is kept, not copied.
	 *
	 * @param requires	the required members' files (a mutable list, which becomes the live list), or {@code null}
	 * 					for none
	 */
	void setRequiresAttribute(@Nullable List<String> requires) {
		this.requires = (requires == null) ? new ArrayList<>() : requires;
	}

	/**
	 * Indicates if a run's test strategy override must not apply to this member. Set-up and restore members are always
	 * locked; an ordinary member is locked when it says so (see {@link OrdinarySuiteMember#getLockTestStrategy()}).
	 *
	 * @return {@code true} for a set-up or restore member
	 */
	@XmlTransient
	@SuppressWarnings("static-method") // OrdinarySuiteMember overrides it with its attribute
	public boolean isTestStrategyLocked() {
		return true;
	}
}
