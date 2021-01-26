#include "IncludeFixer.h"
#include "clang/Format/Format.h"

namespace format = clang::format;
namespace tooling = clang::tooling;

static tooling::Replacements CreateHeaderInsertionForFile(
    llvm::StringRef source_path, llvm::StringRef include) {
  return tooling::Replacements{{source_path, UINT_MAX, 0, include}};
}
static tooling::Replacements CreateHeaderDeletionForFile(
    llvm::StringRef source_path, llvm::StringRef include) {
  return tooling::Replacements{{source_path, UINT_MAX, 1, include}};
}

static const std::string kOwnerInclude = "#include \"gpos/common/owner.h\"";
static const std::string kRefInclude = "#include \"gpos/common/Ref.h\"";

namespace orca_tidy {
bool IncludeFixer::BeginSourceFileAction(clang::CompilerInstance& CI) {
  auto source_path = getCurrentFile();
  tooling::Replacements replacements;
  switch (mode) {
    default:
      llvm_unreachable("Unreachable: unknown FixIncludeMode");
    case kOwner: {
      replacements = CreateHeaderInsertionForFile(source_path, kOwnerInclude);
      break;
    }
    case kRef: {
      replacements =
          CreateHeaderInsertionForFile(source_path, kRefInclude)
              .merge(CreateHeaderDeletionForFile(source_path, kOwnerInclude));
      break;
    }
  }

  auto buffer = llvm::MemoryBuffer::getFile(source_path);
  if (!buffer) {
    llvm::errs() << "Couldn't open file: " + source_path + ": "
                 << buffer.getError().message() + '\n';
    return false;
  }
  llvm::StringRef code = buffer.get()->getBuffer();
  auto style = format::getStyle(format::DefaultFormatStyle, source_path,
                                format::DefaultFallbackStyle);
  if (!style) {
    llvm::errs() << llvm::toString(style.takeError()) << '\n';
    return false;
  }
  auto clean_replaces =
      format::cleanupAroundReplacements(code, replacements, *style);

  if (!clean_replaces) {
    llvm::errs() << llvm::toString(clean_replaces.takeError()) << '\n';
    return false;
  }

  replacements = std::move(clean_replaces.get());

  file_to_replaces.emplace(source_path, std::move(replacements));

  return true;
}
IncludeFixer::IncludeFixer(
    std::map<std::string, tooling::Replacements>& file_to_replaces,
    FixIncludeMode mode)
    : file_to_replaces(file_to_replaces), mode(mode) {}

std::unique_ptr<clang::FrontendAction> IncludeFixerActionFactory::create() {
  return std::unique_ptr<clang::FrontendAction>(
      new IncludeFixer(file_to_replaces, fix_include_mode));
}

IncludeFixerActionFactory::IncludeFixerActionFactory(
    std::map<std::string, clang::tooling::Replacements>& file_to_replaces,
    FixIncludeMode fix_include_mode)
    : file_to_replaces(file_to_replaces), fix_include_mode(fix_include_mode) {}
}  // namespace orca_tidy
