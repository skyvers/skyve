package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation.TestStrategy;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL suite: an execution plan of independent members, each a {@link Workflow} file (an {@link Automation} file is
 * a workflow of one), for example
 * <pre>
 * &lt;suite xmlns="http://www.skyve.org/xml/sail" testStrategy="Assert" evidence="video"&gt;
 * 	&lt;viewRoles user="sail{run}"&gt;
 * 		&lt;role&gt;admin.BasicUser&lt;/role&gt;
 * 	&lt;/viewRoles&gt;
 * 	&lt;setUp file="00-precondition.xml"/&gt;
 * 	&lt;setUp file="00-backup.xml"/&gt;
 * 	&lt;member file="01-session.xml"/&gt;
 * 	&lt;member file="04-devops.xml" requires="03-security-admin.xml" lockTestStrategy="true"/&gt;
 * 	&lt;restore file="99-restore.xml" requires="00-precondition.xml 00-backup.xml"/&gt;
 * &lt;/suite&gt;
 * </pre>
 *
 * <p>A suite runs its set-up members in order, then its ordinary members in order, then its optional restore. Each
 * member gets its own run: members share no {@code {run}} token and no {@code {var:name}} values. A member runs only if
 * every member it requires passed; every ordinary member requires every set-up member without naming them (see
 * {@link #requirementsOf(SuiteMember)}). A failed ordinary member does not stop the others. The restore runs last, even
 * after failures, if its own requirements passed.
 *
 * <p>{@code testStrategy} is a default for the automations of every member that states none; an automation's own
 * attribute, then its workflow's, wins (see {@link #getEffectiveTestStrategy(Workflow, Automation)}). {@code evidence}
 * is a request for the run's evidence, which the run can override. {@code viewRoles} register the roles that the
 * runner builds views with for the users the members sign in as.
 *
 * <p>Like a workflow, a suite is not an {@link Executable}: a runner such as the interpreted harness's
 * {@code util.sail.SailSuite} resolves each member's file relative to the suite file's folder and runs it. Unmarshal one
 * with {@link XMLMetaData#unmarshalSAILSuiteFile(String)}, which also calls {@link #validate(String)}.
 *
 * <p>Not thread-safe: a suite is built, or unmarshalled, before it runs.
 *
 * @see SuiteMember
 * @see OrdinarySuiteMember
 * @see ViewRoles
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"testStrategy", "evidence", "viewRoles", "setUps", "members", "restore"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Suite {
	/**
	 * The evidence that a suite asks its run to keep. A run can override the request.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum Evidence {
		/** Keep no evidence. */
		none,
		/** Keep a failure's screenshot and page source. */
		failures,
		/** Keep the failures, and a snapshot of every step. */
		snapshots,
		/** Keep the failures, and a video of every automation. */
		video,
		/** Keep the failures, the snapshots and the videos. */
		all
	}

	private TestStrategy testStrategy;
	private Evidence evidence;
	private final List<ViewRoles> viewRoles = new ArrayList<>();
	private final List<SuiteMember> setUps = new ArrayList<>();
	private final List<OrdinarySuiteMember> members = new ArrayList<>();
	private SuiteMember restore;

	/**
	 * Returns the default test strategy for the automations of every member that states none.
	 *
	 * @return the default, or {@code null} if the suite states none
	 */
	public @Nullable TestStrategy getTestStrategy() {
		return testStrategy;
	}

	/**
	 * Sets the default test strategy for the automations of every member that states none.
	 *
	 * @param testStrategy	the default, or {@code null} for none
	 */
	@XmlAttribute(name = "testStrategy")
	public void setTestStrategy(@Nullable TestStrategy testStrategy) {
		this.testStrategy = testStrategy;
	}

	/**
	 * Returns the evidence that the suite asks its run to keep.
	 *
	 * @return the request, or {@code null} if the suite makes none (the run decides)
	 */
	public @Nullable Evidence getEvidence() {
		return evidence;
	}

	/**
	 * Sets the evidence that the suite asks its run to keep.
	 *
	 * @param evidence	the request, or {@code null} to leave it to the run
	 */
	@XmlAttribute(name = "evidence")
	public void setEvidence(@Nullable Evidence evidence) {
		this.evidence = evidence;
	}

	/**
	 * Returns the roles that the runner builds views with, one entry per user.
	 *
	 * @return the view roles, in the order written (a live, possibly empty list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "viewRoles")
	public @Nonnull List<ViewRoles> getViewRoles() {
		return viewRoles;
	}

	/**
	 * Returns the set-up members, which run first, in order, locked to their own test strategy.
	 *
	 * @return the set-up members (a live, possibly empty list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "setUp")
	public @Nonnull List<SuiteMember> getSetUps() {
		return setUps;
	}

	/**
	 * Returns the ordinary members, which run in order after the set-up members.
	 *
	 * @return the ordinary members (a live list; at least one in a valid suite)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "member", required = true)
	public @Nonnull List<OrdinarySuiteMember> getMembers() {
		return members;
	}

	/**
	 * Returns the restore member, which runs last, even after failures, if its own requirements passed. It is locked
	 * to its own test strategy.
	 *
	 * @return the restore member, or {@code null} if there is none
	 */
	public @Nullable SuiteMember getRestore() {
		return restore;
	}

	/**
	 * Sets the restore member.
	 *
	 * @param restore	the restore member, or {@code null} for none
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "restore")
	public void setRestore(@Nullable SuiteMember restore) {
		this.restore = restore;
	}

	/**
	 * Returns every member in run order: the set-up members, the ordinary members, then the restore.
	 *
	 * @return a new, unmodifiable list
	 */
	@XmlTransient
	public @Nonnull List<SuiteMember> getAllMembers() {
		List<SuiteMember> result = new ArrayList<>(setUps.size() + members.size() + 1);
		result.addAll(setUps);
		result.addAll(members);
		if (restore != null) {
			result.add(restore);
		}
		return Collections.unmodifiableList(result);
	}

	/**
	 * Returns the member with a file, as other members' {@code requires} name it.
	 *
	 * <p>Complexity: O(m) time, where m is the number of members.
	 *
	 * @param file	the member's file, exactly as written
	 * @return the set-up, ordinary or restore member, or {@code null} if no member has the file
	 */
	public @Nullable SuiteMember getMember(@Nonnull String file) {
		for (SuiteMember member : getAllMembers()) {
			if (file.equals(member.getFile())) {
				return member;
			}
		}
		return null;
	}

	/**
	 * Indicates if a member is one of this suite's set-up members.
	 *
	 * @param member	the member
	 * @return {@code true} if it is a set-up member (by identity)
	 */
	public boolean isSetUp(@Nonnull SuiteMember member) {
		for (SuiteMember setUp : setUps) {
			if (setUp == member) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the members that must pass before a member runs: those its {@code requires} names and, for an ordinary
	 * member, every set-up member. A runner skips the member if any of them failed or was skipped.
	 *
	 * <p>Precondition: the suite is valid ({@link #validate(String)}) and holds the member.
	 *
	 * @param member	a member of this suite
	 * @return the required members in run order, each once (a new, unmodifiable list; empty if there are none)
	 */
	public @Nonnull List<SuiteMember> requirementsOf(@Nonnull SuiteMember member) {
		Set<String> required = new HashSet<>(member.getRequires());
		boolean ordinary = members.contains(member);
		List<SuiteMember> result = new ArrayList<>();
		for (SuiteMember candidate : getAllMembers()) {
			if ((ordinary && isSetUp(candidate)) || required.contains(candidate.getFile())) {
				result.add(candidate);
			}
		}
		return Collections.unmodifiableList(result);
	}

	/**
	 * Returns the test strategy that an automation of one of this suite's members runs with before any run override:
	 * the automation's own, else its workflow's default, else this suite's default.
	 *
	 * @param workflow	the member's workflow
	 * @param automation	one of the workflow's automations
	 * @return the strategy, or {@code null} if none of them states one (the runner's default applies)
	 * @see Workflow#getEffectiveTestStrategy(Automation)
	 */
	public @Nullable TestStrategy getEffectiveTestStrategy(@Nonnull Workflow workflow, @Nonnull Automation automation) {
		TestStrategy result = workflow.getEffectiveTestStrategy(automation);
		return (result == null) ? testStrategy : result;
	}

	/**
	 * Checks what the schema cannot: that every member has a file and no file is listed twice; that every
	 * {@code requires} names another member of the suite that runs before it, without a cycle; that the restore
	 * requires only set-up members; that there is at least one ordinary member; and that each view roles entry has a
	 * user, given once, and non-blank roles.
	 *
	 * <p>Complexity: O(m + r) time, where m is the number of members and r the number of requirements.
	 *
	 * @param name	the suite's name for messages, usually its file or resource
	 * @throws MetaDataException	naming the suite and the member at fault, at the first problem found
	 */
	public void validate(@Nonnull String name) {
		String prefix = "SAIL suite " + name + ": ";
		validateViewRoles(prefix);
		if (members.isEmpty()) {
			throw new MetaDataException(prefix + "there is no <member>");
		}

		List<SuiteMember> all = getAllMembers();
		int[][] requirements = requirements(prefix, all, positions(prefix, all));
		validateNoCycle(prefix, all, requirements);
		for (int i = 0, l = all.size(); i < l; i++) {
			for (int p : requirements[i]) {
				if (p > i) {
					throw new MetaDataException(prefix + describe(i, all.get(i)) + " requires " + all.get(p).getFile() +
													", which runs after it (" + describe(p, all.get(p)) + "); members run in order");
				}
			}
		}
	}

	/**
	 * Returns the run order position of each member's file, throwing if a file is missing or listed twice.
	 */
	private @Nonnull Map<String, Integer> positions(@Nonnull String prefix, @Nonnull List<SuiteMember> all) {
		Map<String, Integer> result = new HashMap<>();
		for (int i = 0, l = all.size(); i < l; i++) {
			String file = all.get(i).getFile();
			if (blank(file)) {
				throw new MetaDataException(prefix + "a <" + element(i) + "> has no file");
			}
			if (result.put(file, Integer.valueOf(i)) != null) {
				throw new MetaDataException(prefix + file + " is listed more than once");
			}
		}
		return result;
	}

	/**
	 * Returns the run order positions of each member's requirements, throwing if one is unknown, the member itself, or
	 * a restore's requirement that is not a set-up member.
	 */
	private @Nonnull int[][] requirements(@Nonnull String prefix,
											@Nonnull List<SuiteMember> all,
											@Nonnull Map<String, Integer> positions) {
		int[][] result = new int[all.size()][];
		for (int i = 0, l = all.size(); i < l; i++) {
			SuiteMember member = all.get(i);
			List<String> requires = member.getRequires();
			int[] memberRequirements = new int[requires.size()];
			for (int j = 0, m = requires.size(); j < m; j++) {
				String required = requires.get(j);
				Integer position = positions.get(required);
				if (position == null) {
					throw new MetaDataException(prefix + describe(i, member) + " requires " + required +
													", which is not a member of the suite");
				}
				int p = position.intValue();
				if (p == i) {
					throw new MetaDataException(prefix + describe(i, member) + " requires itself");
				}
				if ((member == restore) && (p >= setUps.size())) {
					throw new MetaDataException(prefix + describe(i, member) + " requires " + required +
													", which is not a set-up member; a restore can require only set-up members");
				}
				memberRequirements[j] = p;
			}
			result[i] = memberRequirements;
		}
		return result;
	}

	private void validateViewRoles(@Nonnull String prefix) {
		Set<String> users = new HashSet<>();
		for (ViewRoles entry : viewRoles) {
			String user = entry.getUser();
			if (blank(user)) {
				throw new MetaDataException(prefix + "a <viewRoles> has no user");
			}
			if (! users.add(user)) {
				throw new MetaDataException(prefix + "<viewRoles> for user " + user + " is given more than once");
			}
			List<String> roles = entry.getRoles();
			if (roles.isEmpty()) {
				throw new MetaDataException(prefix + "<viewRoles> for user " + user + " has no <role>");
			}
			for (String role : roles) {
				if (blank(role)) {
					throw new MetaDataException(prefix + "<viewRoles> for user " + user + " has a blank <role>");
				}
			}
		}
	}

	/**
	 * Throws if the requirements form a cycle, naming it. A depth first search, O(m + r).
	 */
	private void validateNoCycle(@Nonnull String prefix, @Nonnull List<SuiteMember> all, @Nonnull int[][] requirements) {
		// 0 = not visited, 1 = on the current path, 2 = done
		int[] states = new int[all.size()];
		List<Integer> path = new ArrayList<>();
		for (int i = 0, l = all.size(); i < l; i++) {
			if (states[i] == 0) {
				visit(prefix, all, requirements, i, states, path);
			}
		}
	}

	private void visit(@Nonnull String prefix,
						@Nonnull List<SuiteMember> all,
						@Nonnull int[][] requirements,
						int index,
						@Nonnull int[] states,
						@Nonnull List<Integer> path) {
		states[index] = 1;
		path.add(Integer.valueOf(index));
		for (int p : requirements[index]) {
			if (states[p] == 1) {
				StringBuilder cycle = new StringBuilder(64);
				for (int j = path.indexOf(Integer.valueOf(p)), l = path.size(); j < l; j++) {
					cycle.append(all.get(path.get(j).intValue()).getFile()).append(" -> ");
				}
				cycle.append(all.get(p).getFile());
				throw new MetaDataException(prefix + describe(index, all.get(index)) + " is in a cycle of requires: " + cycle);
			}
			if (states[p] == 0) {
				visit(prefix, all, requirements, p, states, path);
			}
		}
		path.remove(path.size() - 1);
		states[index] = 2;
	}

	/**
	 * Returns the element name of the member at a run order position.
	 */
	private @Nonnull String element(int position) {
		if (position < setUps.size()) {
			return "setUp";
		}
		return (position < setUps.size() + members.size()) ? "member" : "restore";
	}

	/**
	 * Describes the member at a run order position for a message, for example "setUp 00-backup.xml".
	 */
	private @Nonnull String describe(int position, @Nonnull SuiteMember member) {
		return "<" + element(position) + "> " + member.getFile();
	}

	private static boolean blank(@Nullable String value) {
		return (value == null) || value.isBlank();
	}
}
