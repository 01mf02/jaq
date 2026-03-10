# Contribution guidelines

Any contribution must preserve
quality,
maintainability, and
usability of this project.

To ensure this, you must:

- Write a good description:
  Provide your personal motivation for changes you propose.
  Your description should aim to be like the documentation:
  comprehensible, concise, and complete.
- Respect the values:
  Changes must adhere to the goals set out in the README, i.e. they must preserve
  correctness, performance, and simplicity of the code.
- Keep it minimal:
  Changes must be minimal to allow others to verify them in reasonable time.
  The likelihood of a contribution being accepted is inversely proportional to its size.
- Take your time:
  Achieving high quality takes time.
  As a rough guideline,
  writing new content takes me about 10 to 100 times longer than reading it.
  The ratio for your changes should be roughly in the same order of magnitude.
- Document: Document user-observable changes in `docs/`.
- Test:
  Add unit or shell tests to the documentation if they might interest users.
  Include more exotic tests in the test suites of the respective crates.

You must not:

- Duplicate code:
  Code duplication is a serious problem for long-term project maintenance.
  Use existing project infrastructure wherever possible, and do not copy-paste.
- Add dependencies:
  Dependencies take effort to maintain, in particular to validate and to update.
  Therefore, new dependencies may only be added in exceptional cases.
  All dependencies must be compatible with the spirit of this project and
  add significant value to it.
- Include machine-generated content:
  All contributed content, such as description, code, and documentation,
  must be human-written by the contributor, to ensure its quality.

Thank you for respecting these guidelines!
I am looking forward to your contributions.
