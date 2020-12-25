#include "IncludeFixer.h"
#include "clang/Format/Format.h"

namespace format = clang::format;
namespace tooling = clang::tooling;

static tooling::Replacements CreateHeaderInsertionForFile(
    llvm::StringRef source_path) {
  return tooling::Replacements{
      {source_path, UINT_MAX, 0, "#include \"gpos/common/owner.h\""}};
}

namespace orca_tidy {
bool IncludeFixer::BeginSourceFileAction(clang::CompilerInstance& CI) {
  auto source_path = getCurrentFile();
  tooling::Replacements insertion = CreateHeaderInsertionForFile(source_path);
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
      format::cleanupAroundReplacements(code, insertion, *style);

  if (!clean_replaces) {
    llvm::errs() << llvm::toString(clean_replaces.takeError()) << '\n';
    return false;
  }

  insertion = std::move(clean_replaces.get());

  file_to_replaces.emplace(source_path, std::move(insertion));

  return true;
}
IncludeFixer::IncludeFixer(
    std::map<std::string, tooling::Replacements>& file_to_replaces)
    : file_to_replaces(file_to_replaces) {}

std::unique_ptr<clang::FrontendAction> IncludeFixerActionFactory::create() {
  return std::unique_ptr<clang::FrontendAction>(
      new IncludeFixer(file_to_replaces));
}

IncludeFixerActionFactory::IncludeFixerActionFactory(
    std::map<std::string, clang::tooling::Replacements>& file_to_replaces)
    : file_to_replaces(file_to_replaces) {}
}  // namespace orca_tidy
